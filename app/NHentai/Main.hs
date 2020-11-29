{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.QSem
import Control.Error
import Control.Lens hiding (Context)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.State
import Data.NHentai.API.Gallery
import Data.NHentai.Scraper.HomePage
import Data.NHentai.Types
import Data.Time
import Data.Time.Format.ISO8601
import NHentai.Options
import NHentai.Utils
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Options.Applicative
import Refined
import Streaming (Stream, Of)
import System.Directory
import System.IO (openFile)
import System.Random
import Text.HTML.Scalpel.Core
import Text.URI
import UnliftIO hiding (catch)
import UnliftIO.Concurrent
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Streaming as S
import qualified Streaming.Prelude as S

data Context
	= Context
		{ _ctxLeafSem :: QSem
		, _ctxBranchSem :: QSem
		}

makeLenses ''Context

initContext :: MonadIO m => Refined Positive Int -> Refined Positive Int -> m Context
initContext leaf_threads branch_threads = Context
	<$> (liftIO . newQSem . unrefine $ leaf_threads)
	<*> (liftIO . newQSem . unrefine $ branch_threads)

data SmartHttpState =
	SmartHttpState
		{ _retryCounter :: Int
		}

makeLenses ''SmartHttpState

initSmartHttpState :: SmartHttpState
initSmartHttpState = SmartHttpState 0

smartHttpLbs :: (MonadCatch m, MonadLoggerIO m) => Manager -> Request -> m (Response BL.ByteString)
smartHttpLbs mgr req = do
	let uri_text = tshow . getUri $ req
	$logDebug $ "Downloading " <> uri_text
	(dt, rep) <- withTimer $ (flip evalStateT) initSmartHttpState $ fix $ \loop -> do
		rep <- (liftIO $ httpLbs req mgr) `catch` \(e :: SomeException) -> do
			counter <- retryCounter <+= 1
			$logDebug $ (tshow . getUri $ req) <> " failed (Counter: " <> tshow counter <> "), retrying + reduce download speed"
			liftIO (randomRIO (0, counter * 100000)) >>= threadDelay
			loop

		if responseStatus rep == serviceUnavailable503 then do
			$logDebug $ (tshow . getUri $ req) <> " failed: Status 503, redownloading"
			loop
		else do
			pure rep
	$logDebug $ "Done downloading " <> uri_text <> ", content size: " <> tshow (BL.length . responseBody $ rep) <> ", time taken: " <> tshow dt
	pure rep

httpThenSaveAsIfMissing :: (MonadCatch m, MonadLoggerIO m) => Manager -> FilePath -> Request -> m ()
httpThenSaveAsIfMissing mgr file_path req = do
	file_exists <- liftIO $ doesFileExist file_path
	if file_exists then do
		$logDebug $ "File " <> (T.pack file_path) <> " exists, skip downloading " <> (tshow . getUri $ req)
	else do
		$logDebug $ "File " <> (T.pack file_path) <> " does not exist, saving + downloading " <> (tshow . getUri $ req)
		rep <- smartHttpLbs mgr req
		mkParentDirectoryIfMissing file_path
		liftIO $ BL.writeFile file_path (responseBody rep)

readOrHttpThenSaveAsIfMissing :: (MonadCatch m, MonadLoggerIO m) => Manager -> FilePath -> Request -> m BL.ByteString
readOrHttpThenSaveAsIfMissing mgr file_path req = do
	file_exists <- liftIO $ doesFileExist file_path
	if file_exists then do
		$logDebug $ "File " <> (T.pack file_path) <> " exists, skip downloading " <> (tshow . getUri $ req)
		liftIO $ BL.readFile file_path
	else do
		$logDebug $ "File " <> (T.pack file_path) <> " does not exist, saving + downloading " <> (tshow . getUri $ req)
		rep <- smartHttpLbs mgr req
		mkParentDirectoryIfMissing file_path
		liftIO $ BL.writeFile file_path (responseBody rep)
		pure $ responseBody rep

getLatestGid :: (MonadCatch m, MonadLoggerIO m) => Manager -> m GalleryId 
getLatestGid mgr = do
	req <- mkHomePageUri $$(refineTH 1) >>= requestFromModernURI
	body <- responseBody <$> smartHttpLbs mgr req
	case scrapeStringLike body homePageScraper of
		Nothing -> throwM (ScalpelException body)
		Just home_page -> pure $ home_page ^. recentGalleries . head1 . galleryId

getGidInputStream :: (MonadUnliftIO m, MonadLoggerIO m, MonadCatch m) => GidInputOption -> S.Stream (Of GalleryId) m ()
getGidInputStream (GidInputOptionSingle gid) = S.yield gid
getGidInputStream (GidInputOptionListFile file_path) = do
	h <- liftIO $ openFile file_path ReadMode
	S.mapMaybeM parse . enumerate . S.filter (not . null) . S.fromHandle $ h
	hClose h
	where
	parse (line_at :: Integer, string) = case readMay string of
		Nothing -> do
			$logError $ prefix <> "Unable to parse " <> T.pack (show string) <> " as a gallery id, skipping"
			pure Nothing
		Just unref_gid -> case refineThrow unref_gid of
			Left err -> do
				$logError $ prefix <> "Unable to refine " <> T.pack (show unref_gid) <> " to a gallery id, skipping. Error: " <> T.pack (show err)
				pure Nothing
			Right gid -> pure $ Just (gid :: GalleryId)
		where
		prefix = "In " <> T.pack file_path <> ":" <> T.pack (show line_at) <> ": "

asyncLeaf :: (MonadUnliftIO m) => Context -> m a -> m (Async a)
asyncLeaf ctx f = async $ do
	liftIO $ waitQSem (ctx ^. ctxLeafSem)
	a <- f
	liftIO $ signalQSem (ctx ^. ctxLeafSem)
	pure a

downloadPagesWith :: (MonadCatch m, MonadLoggerIO m, MonadUnliftIO m)
	=> (MediaId -> PageIndex -> ImageType -> m URI)
	-> (Lens' OutputConfig (GalleryId -> MediaId -> PageIndex -> ImageType -> FilePath))
	-> Context
	-> OutputConfig
	-> Manager
	-> ApiGallery
	-> Stream (Of (Async ())) m ()
downloadPagesWith url_maker maker_lens ctx out_cfg mgr g = loop $$(refineTH 1) (g ^. pages)
	where
	loop _ [] = pure ()
	loop pid (page:pages) = lift (refineThrow $ unrefine pid + 1) >>= \pid' -> case page ^. eitherImageType of
		Left ext -> do
			lift $ $logError $ "Gallery " <> (tshow . unrefine $ g ^. galleryId) <> ", page " <> (tshow . unrefine $ pid) <> " has invalid image type: " <> tshow ext <> ", skipping"
		Right imgtype -> do
			req <- lift $ url_maker (g ^. mediaId) pid imgtype >>= requestFromModernURI
			let file_path = (out_cfg ^. maker_lens) (g ^. galleryId) (g ^. mediaId) pid imgtype
			lift (asyncLeaf ctx $ httpThenSaveAsIfMissing mgr file_path req) >>= S.yield
			loop pid' pages


downloadPageThumbnails :: (MonadCatch m, MonadLoggerIO m, MonadUnliftIO m) => Context -> OutputConfig -> Manager -> ApiGallery -> Stream (Of (Async ())) m ()
downloadPageThumbnails = downloadPagesWith mkPageThumbnailUri pageThumbnailPathMaker

downloadPageImages :: (MonadCatch m, MonadLoggerIO m, MonadUnliftIO m) => Context -> OutputConfig -> Manager -> ApiGallery -> Stream (Of (Async ())) m ()
downloadPageImages = downloadPagesWith mkPageImageUri pageImagePathMaker

downloadGalleries :: (MonadCatch m, MonadLoggerIO m, MonadUnliftIO m) => Context -> OutputConfig -> DownloadOptions -> Manager -> S.Stream (Of GalleryId) m () -> m ()
downloadGalleries ctx out_cfg down_opts mgr stream = S.uncons stream >>= \case
	Nothing -> pure ()
	Just (gid, stream') -> do
		liftIO $ waitQSem (ctx ^. ctxBranchSem)
		remaining_tasks <- async $ downloadGalleries ctx out_cfg down_opts mgr stream'

		$logInfo $ "Fetching gallery " <> (tshow . unrefine $ gid)
		g_req <- mkApiGalleryUri gid >>= requestFromModernURI
		tasks <- A.eitherDecode <$> readOrHttpThenSaveAsIfMissing mgr (out_cfg ^. jsonPathMaker $ gid) g_req >>= \case
			Left _ -> pure []
			Right (ApiGalleryResultError err) -> do
				$logError $ "Gallery " <> (tshow . unrefine $ gid) <> " gives out an error: " <> tshow err
				pure []
			Right (ApiGalleryResultSuccess g) -> S.toList_ $ do
				when (down_opts ^. downloadPageThumbnailFlag) $ downloadPageThumbnails ctx out_cfg mgr g
				when (down_opts ^. downloadPageImageFlag) $ downloadPageImages ctx out_cfg mgr g

		liftIO $ signalQSem (ctx ^. ctxBranchSem)

		dt <- withTimer_ (forM_ tasks wait)
		$logInfo $ "Done fetching gallery " <> (tshow . unrefine $ gid) <> ", time taken: " <> tshow dt

		wait remaining_tasks

runMainOptions :: (MonadCatch m, MonadLoggerIO m, MonadUnliftIO m) => MainOptions -> m ()
runMainOptions MainOptionsVersion = liftIO $ putStrLn "0.1.3.0"
runMainOptions MainOptionsLatestGid = do
	mgr <- newTlsManager
	latest_gid <- getLatestGid mgr
	liftIO $ print (unrefine latest_gid)
runMainOptions (MainOptionsDownload {..}) = do
	mgr <- newTlsManager
	ctx <- initContext mainOptNumLeafThreads mainOptNumBranchThreads
	let gid_stream = getGidInputStream mainOptGidInputOption
	dt <- withTimer_ $ downloadGalleries ctx mainOptOutputConfig mainOptDownloadOptions mgr gid_stream
	$logInfo $ "Finished! Time taken: " <> tshow dt

main :: IO ()
main = do
	options <- execParser $ info (programOptionsParser <**> helper)
		( fullDesc
		<> progDesc "A scraper/downloader for nhentai.net"
		)
	let filtered = filterLogger
		(\_ level -> case maybeLogLevel'ProgramOptions options of
			Nothing -> False
			Just level' -> level' <= level
		)
		$ runMainOptions (mainOptions'ProgramOptions options)
	runLoggingT filtered $ \loc source level logstr -> do
		let lvl_name = toLogStr $ drop 5 (show level)
		t <- getCurrentTime
		let line = toLogStr (iso8601Show t) <> ": " <> lvl_name <> ": " <> toLogStr logstr <> "\n"
		BSC.hPutStr stderr $ fromLogStr line

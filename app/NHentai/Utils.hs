{-# LANGUAGE LambdaCase #-}

module NHentai.Utils where

import Control.Error
import Control.Exception hiding (catch, mask)
import System.Directory
import System.FilePath
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.List
import Data.List.Split
import Data.Time.Clock.POSIX
import Network.HTTP.Client
import Options.Applicative
import Options.Applicative.Types
import Refined
import Streaming (Stream, Of)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as L
import qualified Data.Text as T
import qualified Network.URI as U
import qualified Streaming.Prelude as S
import qualified Text.URI as URI

data ScalpelException
	= ScalpelException
		{ input'ScalpelException :: BL.ByteString
		}
	deriving (Show, Eq)
instance Exception ScalpelException

data ReadException = ReadException { input'ReadException :: String, part'ReadException :: String }
	deriving (Show, Eq)
instance Exception ReadException

refineReadM :: (Read x, Predicate p x) => ReadM (Refined p x)
refineReadM = eitherReader $ \string -> do
	case readMay string of
		Nothing -> Left $ "unable to parse string: " <> show string
		Just x -> refine x & _Left %~ show

listReadM :: ReadM x -> ReadM [x]
listReadM (ReadM reader') = ReadM . ReaderT $ traverse (runReaderT reader') . splitOn ","

nonEmptyReadM :: ReadM x -> ReadM (L.NonEmpty x)
nonEmptyReadM readm = do
	(L.nonEmpty <$> listReadM readm) >>= \case
		Nothing -> fail "parsed list is empty"
		Just x -> pure x

logLevelReadM :: ReadM LogLevel
logLevelReadM = eitherReader $ \string -> do
	case lookup string my_map of
		Nothing -> Left $ "must be one of: " <> (intercalate ", " $ fmap fst my_map) <> ", but received: " <> show string
		Just x -> pure x
	where
	my_map =
		[ ("debug", LevelDebug)
		, ("info", LevelInfo)
		, ("warn", LevelWarn)
		, ("error", LevelError)
		]

withTimer :: MonadIO m => m a -> m (POSIXTime, a)
withTimer f = do
	t <- liftIO getPOSIXTime
	a <- f
	t' <- liftIO getPOSIXTime
	pure (t' - t, a)

withTimer_ :: MonadIO m => m a -> m POSIXTime
withTimer_ = fmap fst . withTimer

enumerate :: (Integral i, Monad m) => Stream (Of a) m r -> Stream (Of (i, a)) m r
enumerate = S.zip (S.enumFrom 1)

requestFromModernURI :: MonadThrow m => URI.URI -> m Request
requestFromModernURI = parseRequest . URI.renderStr

uritoModernUri :: MonadThrow m => U.URI -> m URI.URI
uritoModernUri = URI.mkURI . T.pack . show

mkParentDirectoryIfMissing :: MonadIO m => FilePath -> m ()
mkParentDirectoryIfMissing = liftIO . createDirectoryIfMissing True . takeDirectory

tshow :: Show a => a -> T.Text
tshow = T.pack . show

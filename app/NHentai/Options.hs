{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module NHentai.Options where

import Control.Lens
import Control.Monad.Logger
import Data.NHentai.Types
import NHentai.Utils
import Options.Applicative
import Refined
import System.FilePath

data DownloadOptions
	= DownloadOptions
		{ _downloadPageThumbnailFlag :: Bool
		, _downloadPageImageFlag :: Bool
		}
	deriving (Show, Eq)

makeClassy ''DownloadOptions

downloadOptionsParser :: Parser DownloadOptions
downloadOptionsParser = DownloadOptions
	<$> download_page_thumbnail_parser
	<*> download_page_image_parser
	where
	download_page_thumbnail_parser = switch
		( short 'T'
		<> long "thumbnails"
		<> help "Download page thumbnails of a gallery"
		)
	download_page_image_parser = switch
		( short 'I'
		<> long "images"
		<> help "Download page images of a gallery"
		)

data OutputConfig
	= OutputConfig
		{ _jsonPathMaker :: GalleryId -> FilePath
		, _pageThumbnailPathMaker :: GalleryId -> MediaId -> PageIndex -> ImageType -> FilePath
		, _pageImagePathMaker :: GalleryId -> MediaId -> PageIndex -> ImageType -> FilePath
		}

makeClassy ''OutputConfig

simpleOutputConfig :: (GalleryId -> FilePath) -> OutputConfig
simpleOutputConfig prefix = OutputConfig
	{ _jsonPathMaker = \gid -> prefix gid </> "gallery.json"
	, _pageThumbnailPathMaker = \gid _ pid img_type -> prefix gid </> (show (unrefine pid) <> "t." <> extension # img_type)
	, _pageImagePathMaker = \gid _ pid img_type -> prefix gid </> (show (unrefine pid) <> "." <> extension # img_type)
	}

-- |The directory name will be the same as the gallery id
mkDefaultOutputConfig :: FilePath -> OutputConfig
mkDefaultOutputConfig output_dir = simpleOutputConfig $ \gid -> let unref_gid = unrefine gid in output_dir </> show unref_gid

-- |I don't really know how to explain this in English but here are some examples:
-- 344013 -> 344/344013
-- 177013 -> 177/177013
-- 9502 -> 9/502
-- 34 -> 0/34
mkDefaultOutputConfig2 :: FilePath -> OutputConfig
mkDefaultOutputConfig2 output_dir = simpleOutputConfig $ \gid -> let unref_gid = unrefine gid in output_dir </> show (unref_gid `div` 1000) </> show unref_gid

outputConfigParser :: Parser OutputConfig
outputConfigParser = (mk_conf2_parser <|> mk_conf1_parser) <*> output_dir_parser
	where
	mk_conf1_parser = pure mkDefaultOutputConfig
	mk_conf2_parser = flag' mkDefaultOutputConfig2
		( short '2'
		<> long "output-config-2"
		<> help "Use another directory format, instead of gid -> dest_dir/<gid>/, the directory format will become gid -> dest_dir/<div gid 1000>/<gid>"
		)
	output_dir_parser = strOption
		( short 'o'
		<> long "output-dir"
		<> metavar "OUTPUT_DIR"
		<> value "galleries"
		<> showDefault
		<> help "Set the output directory"
		)

data GidInputOption
	= GidInputOptionSingle
		{ _gidOptGalleryId :: GalleryId
		}
	| GidInputOptionListFile
		{ _gidOptFilePath :: FilePath
		}
	deriving (Show, Eq)

gidInputOptionParser :: Parser GidInputOption
gidInputOptionParser = single_parser <|> list_file_parser
	where
	single_parser = GidInputOptionSingle
		<$> option refineReadM
			( short 'g'
			<> long "gallery"
			<> metavar "GALLERY_ID"
			<> help "Which gallery to download"
			)
	list_file_parser = GidInputOptionListFile
		<$> strOption
			( short 'f'
			<> long "list-file"
			<> metavar "FILE_PATH"
			<> help "File path containing the list of galleries to be downloaded, skips invalid lines, can have infinite lines"
			)

data MainOptions
	= MainOptionsDownload
		{ mainOptGidInputOption :: GidInputOption
		, mainOptNumLeafThreads :: Refined Positive Int
		, mainOptNumBranchThreads :: Refined Positive Int
		, mainOptOutputConfig :: OutputConfig
		, mainOptDownloadOptions :: DownloadOptions
		}
	| MainOptionsVersion
	| MainOptionsLatestGid

mainOptionsParser :: Parser MainOptions
mainOptionsParser = subparser
	( main_download_command
	<> main_version_command
	<> main_latest_gid_command
	)
	where
	main_download_command = command "download" $ info (main_download_option <**> helper)
		( fullDesc
		<> progDesc "Download pages of galleries"
		)

	main_download_option = MainOptionsDownload
		<$> gidInputOptionParser
		<*> num_leaf_threads_parser
		<*> num_branch_threads_parser
		<*> outputConfigParser
		<*> downloadOptionsParser
		where
		num_leaf_threads_parser = option refineReadM
			( short 't'
			<> long "leaf-threads"
			<> metavar "NUM_THREADS"
			<> value $$(refineTH @Positive @Int 1)
			<> showDefault
			<> help "Set the number of threads used in downloading"
			)
		num_branch_threads_parser = option refineReadM
			( short 'b'
			<> long "branch-threads"
			<> metavar "NUM_THREADS"
			<> value $$(refineTH @Positive @Int 1)
			<> showDefault
			<> help "Set the number of threads used in downloading"
			)

	main_version_command = command "version" $ do
		info (pure MainOptionsVersion <**> helper)
			( fullDesc
			<> progDesc "Print version"
			)

	main_latest_gid_command = command "latest-gid" $ do
		info (pure MainOptionsLatestGid <**> helper)
			( fullDesc
			<> progDesc "Print the latest gallery's id"
			)

data ProgramOptions
	= ProgramOptions
		{ maybeLogLevel'ProgramOptions :: Maybe LogLevel
		, mainOptions'ProgramOptions :: MainOptions
		}

programOptionsParser :: Parser ProgramOptions
programOptionsParser = ProgramOptions <$> maybe_log_level_parser <*> mainOptionsParser
	where
	maybe_log_level_parser :: Parser (Maybe LogLevel)
	maybe_log_level_parser = (Just <$> f) <|> pure Nothing
		where
		f = option logLevelReadM
			( short 'l'
			<> long "log-level"
			<> metavar "LOG_LEVEL"
			<> help "Set log level (default disables logging). Prints possible inputs on error"
			)

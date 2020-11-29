{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.NHentai.API.Gallery
( mkApiGalleryUri
, mkPageThumbnailUri
, mkPageImageUri

, toTagType

, ApiTag(..)
, tagId
, tagType
, tagName
, tagUri
, tagCount

, ApiGallery(..)
, apiGalleryId
, apiMediaId
, titleEnglish
, titleJapanese
, titlePretty
, pages
, cover
, thumbnail
, scanlator
, uploadDate
, tags
, numPages
, numFavorites

, ApiGalleryResult(..)
)
where

import Control.Applicative
import Control.Error
import Control.Lens
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types
import Data.NHentai.Internal.Utils
import Data.NHentai.Types
import Data.Scientific
import Data.Text.Lens
import Data.Time.Clock
import Refined
import Text.URI (URI)
import Text.URI hiding (URI(..))
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

mkApiGalleryUri :: MonadThrow m => GalleryId -> m URI
mkApiGalleryUri gid = do
	gid_path_piece <- mkPathPiece (show (unrefine gid) ^. packed)
	pure $ prefix & uriPath %~ (<> [gid_path_piece])
	where
	prefix = [uri|https://nhentai.net/api/gallery|]

toTagType :: String -> Maybe TagType
toTagType str = readMay (capitalize str <> "Tag")

newtype ApiTagType = ApiTagType { unApiTagType :: TagType } deriving (Show, Eq)

instance FromJSON ApiTagType where
	parseJSON = withText "ApiTagType" $ \v -> do
		let v' = T.unpack v
		case toTagType v' of
			Just j -> pure $ ApiTagType j
			Nothing -> fail $ "unknown tag type: " <> show v'

-- e.g. https://i.nhentai.net/galleries/900513/2.00 https://nhentai.net/api/gallery/155974
-- e.g. https://nhentai.net/g/155844/24/
newtype ApiImageType = ApiImageType { unApiImageType :: Either String ImageType } deriving (Show, Eq)

instance FromJSON ApiImageType where
	parseJSON = withText "ApiImageType" $ \string_packed -> do
		let string = T.unpack string_packed
		case headMay string of
			Nothing -> fail $ "image type string is empty"
			Just ch -> case ch ^? extensionChar of
				Nothing -> pure . ApiImageType . Left $ string
				Just image_type -> pure . ApiImageType . Right $ image_type

newtype ApiImageSpec = ApiImageSpec { unApiImageSpec :: ImageSpec } deriving (Show, Eq)

instance FromJSON ApiImageSpec where
	parseJSON = withObject "ApiImageSpec" $ \v -> do
		ApiImageSpec <$> (ImageSpec
			<$> (unApiImageType <$> v .: "t")
			<*> (v .: "w" >>= refineFail)
			<*> (v .: "h" >>= refineFail))

data ApiTag
	= ApiTag
		{ _tagId :: TagId
		, _tagType :: TagType
		, _tagName :: T.Text
		, _tagUri :: URI
		, _tagCount :: Refined NonNegative Int
		}
	deriving (Show, Eq)

makeLenses ''ApiTag

instance FromJSON ApiTag where
	parseJSON = withObject "ApiTag" $ \v -> ApiTag
		<$> (v .: "id")
		<*> (unApiTagType <$> v .: "type")
		<*> v .: "name"
		<*> (v .: "url" >>= mkURIFail)
		<*> v .: "count"

data ApiGallery
	= ApiGallery
		{ _apiGalleryId :: GalleryId
		, _apiMediaId :: MediaId
		, _titleEnglish :: T.Text
		, _titleJapanese :: Maybe T.Text
		, _titlePretty :: T.Text
		, _pages :: [ImageSpec]
		, _cover :: ImageSpec
		, _thumbnail :: ImageSpec
		, _scanlator :: T.Text
		, _uploadDate :: UTCTime
		, _tags :: [ApiTag]
		, _numPages :: PageIndex
		, _numFavorites :: Refined NonNegative Int
		}
	deriving (Show, Eq)

makeLenses ''ApiGallery

instance HasGalleryId ApiGallery where
	galleryId = apiGalleryId

instance HasMediaId ApiGallery where
	mediaId = apiMediaId

mkPageThumbnailUri :: MonadThrow m => MediaId -> PageIndex -> ImageType -> m URI
mkPageThumbnailUri mid pid imgtype = do
	mid_pp <- mkPathPiece (show (unrefine mid) ^. packed)
	img_pp <- mkPathPiece (show (unrefine pid) ^. packed <> "t." <> (extension # imgtype) ^. packed)
	pure $ prefix & uriPath %~ (<> [mid_pp, img_pp])
	where
	prefix = [uri|https://t.nhentai.net/galleries|]

mkPageImageUri :: MonadThrow m => MediaId -> PageIndex -> ImageType -> m URI
mkPageImageUri mid pid imgtype = do
	mid_pp <- mkPathPiece (show (unrefine mid) ^. packed)
	img_pp <- mkPathPiece (show (unrefine pid) ^. packed <> "." <> (extension # imgtype) ^. packed)
	pure $ prefix & uriPath %~ (<> [mid_pp, img_pp])
	where
	prefix = [uri|https://i.nhentai.net/galleries|]

intOrString :: (Read a, Integral a) => Value -> Parser a
intOrString (Number i) = case floatingOrInteger @Float i of
	Left _ -> fail "is float"
	Right a -> pure a
intOrString (String x) = readZ (x ^. unpacked)
intOrString _ = fail "neither a String or a Number Int"

data ApiGalleryResult
	= ApiGalleryResultSuccess ApiGallery
	| ApiGalleryResultError T.Text
	deriving (Show, Eq)

instance FromJSON ApiGalleryResult where
	parseJSON = withObject "ApiGalleryResult" $ \v -> error_parser v <|> api_gallery_parser v
		where
		error_parser v = ApiGalleryResultError
			<$> (v .: "error")
		api_gallery_parser v = ApiGalleryResultSuccess <$>
			( ApiGallery
			<$> (v .: "id" >>= intOrString >>= refineFail)
			<*> (v .: "media_id" >>= intOrString >>= refineFail)
			<*> (v .: "title" >>= (.: "english"))
			<*> (v .: "title" >>= (.: "japanese"))
			<*> (v .: "title" >>= (.: "pretty"))
			<*> (fmap unApiImageSpec <$> (v .: "images" >>= (.: "pages")))
			<*> (unApiImageSpec <$> (v .: "images" >>= (.: "cover")))
			<*> (unApiImageSpec <$> (v .: "images" >>= (.: "thumbnail")))
			<*> v .: "scanlator"
			<*> (secondsToUTCTime <$> (v .: "upload_date"))
			<*> v .: "tags"
			<*> v .: "num_pages"
			<*> v .: "num_favorites"
			)

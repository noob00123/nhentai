{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.NHentai.API.Comment
( mkApiCommentUri
, ApiPoster(..)
, posterId
, posterUsername
, posterSlug
, posterAvatarUri
, posterIsSuperUser
, posterIsStaff

, ApiComment(..)
, commentId
, commentGalleryId
, commentPoster
, commentPostDate
, commentBody
)
where

import Control.Lens
import Control.Monad.Catch
import Data.Aeson
import Data.NHentai.API.Gallery
import Data.NHentai.Internal.Utils
import Data.NHentai.Types
import Data.Time.Clock
import Language.Haskell.TH.Syntax
import Refined
import Text.URI (URI)
import Text.URI.Lens
import Text.URI.QQ
import qualified Data.Text as T

mkApiCommentUri :: MonadThrow m => GalleryId -> m URI
mkApiCommentUri gid = do
	uri <- mkApiGalleryUri gid
	pure $ uri & uriPath %~ (<> [[pathPiece|comments|]])

data ApiPoster
	= ApiPoster
		{ _posterId :: PosterId
		, _posterUsername :: T.Text
		, _posterSlug :: T.Text
		, _posterAvatarUri :: URI
		, _posterIsSuperUser :: Bool
		, _posterIsStaff :: Bool
		}
	deriving (Show, Eq)

makeLensesWith (classyRules & lensClass .~ const (Just (mkName "HasApiPoster", mkName "apiPoster"))) ''ApiPoster

instance FromJSON ApiPoster where
	parseJSON = withObject "ApiPoster" $ \v -> ApiPoster
		<$> (v .: "id" >>= refineFail)
		<*> v .: "username"
		<*> v .: "slug"
		<*> ((v .: "avatar_url") >>= mkURIFail)
		<*> v .: "is_superuser"
		<*> v .: "is_staff"

data ApiComment
	= ApiComment
		{ _commentId :: CommentId
		, _commentGalleryId :: GalleryId
		, _commentPoster :: ApiPoster
		, _commentPostDate :: UTCTime
		, _commentBody :: T.Text
		}
	deriving (Show, Eq)

makeLenses ''ApiComment

instance HasApiPoster ApiComment where
	apiPoster = commentPoster

instance FromJSON ApiComment where
	parseJSON = withObject "ApiComment" $ \v -> ApiComment
		<$> (v .: "id" >>= refineFail)
		<*> (v .: "gallery_id" >>= refineFail)
		<*> v .: "poster"
		<*> (secondsToUTCTime <$> (v .: "post_date"))
		<*> v .: "body"

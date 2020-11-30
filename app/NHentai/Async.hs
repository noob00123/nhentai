{-# LANGUAGE TemplateHaskell #-}

module NHentai.Async where

import Control.Concurrent.QSem
import Control.Lens
import Control.Monad.IO.Class
import Refined
import UnliftIO
import UnliftIO.Async

data AsyncContext
	= AsyncContext
		{ _ctxLeafSem :: QSem
		, _ctxBranchSem :: QSem
		}

makeLenses ''AsyncContext

initAsyncContext :: MonadIO m => Refined Positive Int -> Refined Positive Int -> m AsyncContext
initAsyncContext leaf_threads branch_threads = AsyncContext
	<$> (liftIO . newQSem . unrefine $ leaf_threads)
	<*> (liftIO . newQSem . unrefine $ branch_threads)

asyncLeaf :: (MonadUnliftIO m) => AsyncContext -> m a -> m (Async a)
asyncLeaf ctx f = async $ do
	liftIO $ waitQSem (ctx ^. ctxLeafSem)
	a <- f
	liftIO $ signalQSem (ctx ^. ctxLeafSem)
	pure a

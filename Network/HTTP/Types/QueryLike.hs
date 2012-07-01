{-# LANGUAGE FlexibleInstances #-}
module Network.HTTP.Types.QueryLike
(
  QueryLike(toQuery)
)
where

import           Network.HTTP.Types.URI
import           Data.Maybe
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Control.Arrow

-- | Types which can, and commonly are, converted to 'Query' are in this class.
class QueryLike a where
  -- | Convert to 'Query'.
  toQuery :: a -> Query

class QueryKeyLike a where
  toQueryKey :: a -> B.ByteString

class QueryValueLike a where
  toQueryValue :: a -> Maybe B.ByteString

instance (QueryKeyLike k, QueryValueLike v) => QueryLike [(k, v)] where
  toQuery = map (toQueryKey *** toQueryValue)

instance (QueryKeyLike k, QueryValueLike v) => QueryLike [Maybe (k, v)] where
  toQuery = toQuery . catMaybes

instance QueryKeyLike B.ByteString where toQueryKey = id
instance QueryKeyLike L.ByteString where toQueryKey = B.concat . L.toChunks
instance QueryKeyLike T.Text where toQueryKey = T.encodeUtf8
instance QueryKeyLike [Char] where toQueryKey = T.encodeUtf8 . T.pack

instance QueryValueLike B.ByteString where toQueryValue = Just
instance QueryValueLike L.ByteString where toQueryValue = Just . B.concat . L.toChunks
instance QueryValueLike T.Text where toQueryValue = Just . T.encodeUtf8
instance QueryValueLike [Char] where toQueryValue = Just . T.encodeUtf8 . T.pack

instance QueryValueLike a => QueryValueLike (Maybe a) where
  toQueryValue = maybe Nothing toQueryValue
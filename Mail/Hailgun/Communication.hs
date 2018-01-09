{-# LANGUAGE CPP #-}

module Mail.Hailgun.Communication
    ( getRequest
    , postRequest
    , toQueryParams
    , parseResponse
    ) where

import           Control.Arrow                         (second)
import           Control.Monad.Catch                   (MonadThrow (..))
import           Control.Monad.IO.Class                (MonadIO (..))
import           Data.Aeson
import qualified Data.ByteString.Char8                 as BC
import qualified Data.ByteString.Lazy.Char8            as BLC
import           Data.Foldable                         (foldMap)
import           Data.Monoid
import           Mail.Hailgun.Errors
import           Mail.Hailgun.Internal.Data
import qualified Network.HTTP.Client                   as NC
import           Network.HTTP.Client.Internal          (addProxy)
import           Network.HTTP.Client.MultipartFormData (Part (..), formDataBody)
import qualified Network.HTTP.Types.Method             as NM
import qualified Network.HTTP.Types.Status             as NT

toQueryParams :: [(BC.ByteString, BC.ByteString)] -> [(BC.ByteString, Maybe BC.ByteString)]
toQueryParams = fmap (second Just)

getRequest :: (MonadThrow m) => String -> HailgunContext -> [(BC.ByteString, Maybe BC.ByteString)] -> m NC.Request
getRequest url context queryParams = do
   initRequest <- NC.parseUrlThrow url
   let request = appEndo (applyHailgunAuth context) $
         initRequest
            { NC.method = NM.methodGet
#if MIN_VERSION_http_client(0,5,0)
#else
            , NC.checkStatus = ignoreStatus
#endif
            }
   return $ NC.setQueryString queryParams request

postRequest :: (MonadThrow m, MonadIO m) => String -> HailgunContext -> [Part] -> m NC.Request
postRequest url context parts = do
   initRequest <- NC.parseUrlThrow url
   let request = initRequest
         { NC.method = NM.methodPost
#if MIN_VERSION_http_client(0,5,0)
#else
         , NC.checkStatus = ignoreStatus
#endif
         }
   requestWithBody <- formDataBody parts request
   --requestWithBody <- encodeFormData formParams request
   return $ appEndo (applyHailgunAuth context) requestWithBody

applyHailgunAuth :: HailgunContext -> Endo NC.Request
applyHailgunAuth context = addRequestProxy (hailgunProxy context) <> authRequest context

addRequestProxy :: Maybe NC.Proxy -> Endo NC.Request
addRequestProxy = foldMap addProxy'

authRequest :: HailgunContext -> Endo NC.Request
authRequest context = Endo $ NC.applyBasicAuth (BC.pack "api") (BC.pack . hailgunApiKey $ context)

addProxy' :: NC.Proxy -> Endo NC.Request
addProxy' proxy = Endo $ addProxy (NC.proxyHost proxy) (NC.proxyPort proxy)

parseResponse :: (FromJSON a) => NC.Response BLC.ByteString -> Either HailgunErrorResponse a
parseResponse response = statusToResponse . NT.statusCode . NC.responseStatus $ response
   where
      statusToResponse s
         | s == 200                      = responseDecode response
         | s `elem` [400, 401, 402, 404] = gatherErrors . responseDecode $ response
         | s `elem` [500, 502, 503, 504] = serverError
         | otherwise                     = unexpectedError s

responseDecode :: (FromJSON a) => NC.Response BLC.ByteString -> Either HailgunErrorResponse a
responseDecode = mapError . eitherDecode . NC.responseBody

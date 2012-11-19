{-# LANGUAGE OverloadedStrings #-}

module Network.Cosm ( Feed(..)
                    , FeedId
                    , ApiKey
                    , DataStream
                    , update
                    ) where
    
import qualified Data.ByteString.Lazy as BS
import Network.URI
import Network.HTTP
import Data.Aeson

type ApiKey = String
type FeedId = Int
type DataStream = String
    
data Feed = Feed { cosmKey :: ApiKey
                 , cosmFeed :: FeedId
                 }
          deriving (Show, Eq)
              
cosmRequest :: Feed -> RequestMethod -> BS.ByteString -> Request BS.ByteString
cosmRequest feed method body =
    let uri = URI { uriScheme = "http:"
                  , uriAuthority = Just $ URIAuth "" "api.cosm.com" ""
                  , uriPath = "/v2/feeds/"++show (cosmFeed feed)
                  , uriQuery = ""
                  , uriFragment = ""
                  }
    in Request { rqURI = uri
               , rqMethod = method
               , rqHeaders = [ mkHeader (HdrCustom "X-ApiKey") (cosmKey feed)
                             , mkHeader HdrContentLength (show $ BS.length body)
                             ]
               , rqBody = body
               }

update :: Feed -> DataStream -> Double -> IO ()
update feed stream value = do
    let body = encode $ object
               [ "version"      .= ("1.0.0"::String)
               , "datastreams"  .=
                   [ object [ "id"            .= stream
                            , "current_value" .= value
                            ]
                   ]
               ]
    rsp <- simpleHTTP $ cosmRequest feed PUT body
    return ()

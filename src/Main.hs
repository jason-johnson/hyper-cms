module Main where

{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 () -- Just for an orphan instance
import qualified Data.ByteString.Lazy.Char8 as LB8

-- TODO: components should be cached entities at the top level, but make sure no client can get them directly, only internal processes can

app :: Application
app request = case rawPathInfo request of
    "/"     -> return index
    "/raw/" -> return plainIndex
    "/show/" -> return (showRequest request)
    "/auth/" -> return (auth request)
    _       -> return notFound

index :: Response
index = ResponseFile status200 [("Content-Type", "text/html")] "index.html" Nothing

plainIndex :: Response
plainIndex = ResponseFile status200 [("Content-Type", "text/plain")] "index.html" Nothing

showRequest :: Request -> Response
showRequest request = responseLBS status200 [("Content-Type", "text/plain")] $
   "rawPathInfo: " ++< LB8.fromChunks  [rawPathInfo request] ++< "\nrawQueryString: " ++< LB8.fromChunks [rawQueryString request]
   ++< "\nserverName: " ++< LB8.fromChunks [serverName request] ++< showAllHeaders request

auth :: Request -> Response
auth request = 
    case lookup "authorization" (requestHeaders request) of
        Nothing -> responseLBS
                   status401
                   [("Content-Type", "text/plain"),
                    ("WWW-Authenticate", "Digest realm=\"userrealm@localhost\", qop=\"auth,auth-int\", nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"")]
                   "401 - Unauthorized"
        Just _ -> responseLBS
                  status200
                  [("Content-Type", "text/plain")]
                  $ "rawPathInfo: " ++< LB8.fromChunks  [rawPathInfo request]
                  ++< "\nrawQueryString: " ++< LB8.fromChunks [rawQueryString request]
                  ++< "\nserverName: " ++< LB8.fromChunks [serverName request]
                  ++< showAllHeaders request

notFound :: Response
notFound = responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"

-- helpers

(++<) :: LB8.ByteString -> LB8.ByteString -> LB8.ByteString
(++<) l r = LB8.append l r

showAllHeaders :: Request -> LB8.ByteString
showAllHeaders request = LB8.concat $ map combine (requestHeaders request)
   where combine (k,v)  = "\n" ++< packH k ++< ": " ++< packK v
         packH = LB8.pack . show
         packK = LB8.pack . show

-- main

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app

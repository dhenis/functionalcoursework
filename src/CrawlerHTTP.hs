-- |Module that performs http requests as well as parsing returned html.
module CrawlerHTTP where

import Network.HTTP
import Network.HTTP.Conduit
import Network.URI
import Data.Maybe
import Data.Either
import Data.Word
import qualified Data.ByteString.Lazy as L
import Control.Exception
import Language.Haskell.TH.Ppr

type URL = String
type HTML = String

-- |This function will download a given url and return its content as a String
-- This uses simpleHTTP which is not as fast as simpleHttp
downloadURL :: URL              -- String containing the URL to be downloaded
            -> IO String
downloadURL url =
   do resp <- simpleHTTP request
      case resp of
         Left x -> return $ "Error connecting: " ++ show x
         Right r ->
            case rspCode r of
               (2,_,_) -> return $ rspBody r
               _ -> return $ show r
   where
      request = Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
      uri = fromJust $ parseURI url

-- |This function is essentially the same as downloadURL, except that it uses
-- ByteString instead of String
bsDownloadURL :: URL          -- String containing the URL to be downloaded
                -> IO String
bsDownloadURL http = do
   result <- try (simpleHttp http) :: IO (Either HttpException L.ByteString)
   let e = lefts [result]
   if length e == 0 then do
      let text_bs = head.rights $ [result]
      let html_word8 = L.unpack text_bs   :: [Word8]
      return $ bytesToString html_word8   :: IO String
   else
      return ""

-- |Basic method for extracting all urls of a given text (String)
parseURLs :: HTML              -- ^ String representation of html file
          -> [URL]
parseURLs [] = []
parseURLs ('h':'t':'t':'p':':':'/':'/':xs) = ("http://" ++ url) : (parseURLs rest)
   where
      (url, rest) = break space xs
      space c = elem c [' ','\t','\n','\r','\"','\'',')',';','<']
parseURLs (_:xs) = parseURLs xs

isHTML :: URL -> Bool
isHTML = (==".html"). reverse . (take 5) . reverse

getHTMLpages :: HTML -> [URL]
getHTMLpages = filter isHTML . parseURLs

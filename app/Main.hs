module Main where

import CrawlerDB
import CrawlerHTTP
import CrawlerType
import System.Environment

import Data.Maybe
import Control.Exception
import Control.Monad
import Data.Char
import Data.List



-- |The main function provides five different functionalities:
--
--  [@create@] will create the database urls.db
--
--  [@saved@] simply prints all the urls currently stored on the database
--
--  [@unfold@] will download and parse all pages on the database, storing all found links
--
--  [@show url@] will download and display the contents of the given url
--
--  [@crawl url@] will download a given page, parse the links and store on the database
main = do
    conn <- dbConnect
    createDB conn  -- conn <- dbConnection


    putStrLn "Enter city name to check the weather : "
    --name <- getLine
  -- putStrLn $ "Dear " ++ name ++ ", do you wanna be my friend?"
  -- --

--
-- actoreName <- askForActor

--    args <- getArgs
--    case args of
--       ["saved"] -> printURLs conn
--       ["unfold"] -> unfoldDB conn
--       ["show", url] -> do
--             urlText <- downloadURL url
--             print urlText
--       ["crawl", url] -> do
--          urlText <- downloadURL url
--          let urls = getHTMLpages urlText
--          storeURLs conn urls
--       _ -> syntaxError
--    dbDisconnect conn
-- --
-- syntaxError = putStrLn
--    "Usage: Crawler command [args]\n\
--    \\n\
--    \create           Create database urls.db\n\
--    \show url         Shows contents of given URL\n\
--    \saved            List urls on database\n\
--    \crawl url        Gather urls and store in database\n\
--    \unfold           Crawl each of the saved URLs\n"

-- |Module containing functions that operate on the database urls.db
module CrawlerDB where
import CrawlerType
import CrawlerHTTP
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad
import Data.List

-- |Method to create the database urls.db
-- It uses connectSqlite3 from Database.HDBC.Sqlite3

dbConnect :: IO Connection
dbConnect = connectSqlite3 "weather.db"
-- dbConnection = do

   --conn <- connectSqlite3 "weather.db"
   --run conn "CREATE TABLE IF NOT EXISTS urls (url TEXT, processed BOOL)" [] -- active this if you dont have the table
   --commit conn
   -- return conn

createDB::Connection -> IO()
createDB conn = do
  table <- getTables conn
  run conn ("CREATE TABLE IF NOT EXISTS city (id_city integer,city_name VARCHAR(40), lon varchar(40), lat varchar(40),ground_level varchar(40),sea_level varchar(40))") []
  run conn ("CREATE TABLE IF NOT EXISTS weather (id_city integer, conditon varchar(40), description varchar(40), wind_speed varchar(40), wind_deg varchar(40))") []
  run conn ("CREATE TABLE IF NOT EXISTS temp (id_city integer,temp varchar(40), min varchar(40), max varchar(40), preasure varchar(40), humid varchar(40))") []
  commit conn


dbDisconnect :: Connection -> IO ()
dbDisconnect = disconnect


insertCity::Connection->[City]->IO ()
insertCity conn city = do

  let f (City id_city city_name lon lat ground_level sea_level) = [toSql id_city, toSql city_name, toSql lon, toSql lat, toSql ground_level, toSql sea_level]
  let arg = map f city
  stmt <- prepare conn "INSERT INTO city (id_city,city_name, lon, lat ,ground_level ,sea_level ) VALUES (?,?,?,?,?,?)"
  executeMany stmt arg
  commit conn


insertWeather::Connection->[Weather]->IO ()
insertWeather conn weather = do

    let f (Weather id_city conditon description wind_speed wind_deg) = [toSql id_city ,toSql conditon ,toSql description ,toSql wind_speed ,toSql wind_deg]
    let weatherArg = map f weather
    stmt <- prepare conn "INSERT INTO weather (id_city , conditon , description , wind_speed , wind_deg ) VALUES (?,?,?,?,?)"
    executeMany stmt weatherArg
    commit conn


insertTemp::Connection->[Temp]->IO ()
insertTemp conn temp = do

    let f (Temp id_city temp minn maxx preasure humid) = [toSql id_city,toSql temp,toSql minn,toSql maxx,toSql preasure,toSql humid]
    let tempArg = map f temp
    stmt <- prepare conn "INSERT INTO temp (id_city ,temp , minn , maxx , preasure , humid ) VALUES (?,?,?,?,?,?)"
    executeMany stmt tempArg
    commit conn







-- jfklsjdfjsdjlj


urlNotInDB :: Connection -> URL -> IO Bool
urlNotInDB conn url = do
   res <- quickQuery' conn "SELECT url FROM urls WHERE url=(?)" [toSql url]
   return (length res == 0)

-- |Method to store a list of urls into the database (table url)
storeURLs :: Connection
          -> [URL] -- ^ List of URLs to be stored on the database
          -> IO ()
storeURLs _ [] = return ()
storeURLs conn xs = do
   -- removes duplicates or URLs already in database
   xs' <- filterM (urlNotInDB conn) (nub xs)
   stmt <- prepare conn "INSERT INTO urls (url, processed) VALUES (?,?)"
   putStrLn "Adding:"
   mapM_ (\x -> putStrLn $ " - " ++ x) xs'
   executeMany stmt (map (\x -> [toSql x, toSql (0 :: Int)]) xs')
   commit conn

-- |Method to display all the URLs on the database. It uses getURLs.
printURLs :: Connection -> IO ()
printURLs conn = do
   urls <- getURLs conn
   mapM_ print urls

-- |Method to retrieve all the URLs on the database.
getURLs :: Connection -> IO [URL]
getURLs conn = do
   res <- quickQuery' conn "SELECT *   FROM weather" []
   -- asreturn $ map fromSql (map head res)
   return $ map fromSql (map head res)

-- |Method to retrieve all the URLs on the database.
getUnprocessedURLs :: Connection -> IO [URL]
getUnprocessedURLs conn = do
   res <- quickQuery' conn "SELECT url FROM urls WHERE processed=?" [toSql (0 :: Int)]
   return $ map fromSql (map head res)

-- |Method to retrive all the URLs from the database, parse and extract new links
-- from these, and store those links on the database. This method could be made more
-- efficient by remembering which URLs have already been processed, so that next
-- time unfoldDB is called only newly retrieved urls should be processed.
unfoldDB :: Connection -> IO ()
unfoldDB conn = do
   urls <- getUnprocessedURLs conn
   quickQuery' conn "UPDATE urls SET processed=?" [toSql (1 :: Int)]
   process conn urls

-- |Method to donwload a given list of URLs, extract new links and store these on the database.
process :: Connection
        -> [URL]  -- ^ List of URLs to be processed
        -> IO ()
process _ [] = return ()
process conn (x:xs) = do
   print $ "Processing : " ++ x
   urlContent <- downloadURL x
   storeURLs conn (getHTMLpages urlContent)
   process conn xs

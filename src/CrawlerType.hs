-- |Module that performs http requests as well as parsing returned html.
module CrawlerType where


data City = City{id_city ::Int,city_name::String, lon::String, lat ::String,ground_level ::String,sea_level::String  }
  deriving (Eq,Show)

-- instance Show City where
--   show (City _ city_name _ _ _ _ ) = city_name

data Weather = Weather {weather_city::Int, conditon::String, description::String, wind_speed::String, wind_deg::String}
  deriving (Eq, Show)


data Temp = Temp {temp_city::Int, temp::String, minn::String, maxx::String, preasure::String, humid::String}
  deriving (Eq, Show)

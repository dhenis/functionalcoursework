import Data.Aeson as Q
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit
import GHC.Generics
 data weather = weather {
      lon:: Int,
      lat :: ,
      id :: String,
      main :: Maybe String,
      description :: Int, 
      icon :: String,
      temp :: Double,
      pressure :: Int,
      humidity::Int,
      temp_min ::Float,
      temp max :: Float,
      speed::Float,
      deg::Int,
      type::Int,
      message::Float,
      country::String,
      sunrise::Int,
      sunset::Int,
      name::String,}deriving (Show, Generic)
      jsonURL :: String
instance FromJSON City where
   parseJSON (Object v) = lon <$> v .: "lat" <*> v .: "id" <*> v .: "main"<*> v .:"description" <*> v .:"icon" <*> v .:"temp" <*> v .:"pressure" <*> v .:"humidity" <*> v .:"temp_min" <*> v .:"temp max" <*> v .:"speed" <*> v .:"deg" <*> v .:"type" <*> v .:"message" <*> v .:"country" <*> v .:"sunrise" <*> v .:"sunset" <*> v .:"name" <*> v .:
   parseJSON _ = mzero
jsonURL = "http://samples.openweathermap.org/data/2.5/box/city?bbox=12,32,15,37,10&appid=b6907d289e10d714a6e88b30761fae22"
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL


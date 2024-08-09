{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as C8
import Data.Time
import GHC.Generics
import Network.HTTP.Client

data Coords = Coords
  { lon :: Float,
    lat :: Float
  }
  deriving (Generic, Show)

instance ToJSON Coords

instance FromJSON Coords

data SimpleNowcast = SimpleNowcast
  { time :: UTCTime,
    location :: Coords,
    temperature :: Float,
    humidity :: Float,
    wind_speed :: Float,
    wind_dir :: Float
  }
  deriving (Generic, Show)

instance ToJSON SimpleNowcast

instance FromJSON SimpleNowcast

data Nowcast where
  Nowcast :: {simple :: SimpleNowcast} -> Nowcast
  deriving (Generic, Show)

instance ToJSON Nowcast

instance FromJSON Nowcast

firstNowcast :: [Nowcast] -> Nowcast
firstNowcast = Prelude.head

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:3000/api/nowcasts?location=Heimdal"
  response <- httpLbs request manager

  let res = responseBody response

  print res

  let nowcasts = decode res :: Maybe [Nowcast]
  case nowcasts of
    Just nowcasts -> print $ firstNowcast nowcasts
    Nothing -> print "Failed to decode response"

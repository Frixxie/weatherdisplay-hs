{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Aeson
import Data.Time
import GHC.Generics
import Network.HTTP.Client
import System.Console.CmdArgs

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

data CommandOpts = Opts
  { url :: String,
    loc :: String
  }
  deriving (Show, Data, Typeable)

options :: CommandOpts
options = Opts {url = "http://localhost:3000" &= help "Url to use" &= opt "http://localhost:3000", loc = "Heimdal" &= help "Location to lookup" &= opt "Heimdal"} &= summary "Display weather from Wictk"

firstNowcast :: [Nowcast] -> Nowcast
firstNowcast = Prelude.head

printTemp :: Nowcast -> IO ()
printTemp (Nowcast simpleEntry) = print $ temperature simpleEntry

main :: IO ()
main = do
  lopts <- cmdArgs options
  manager <- newManager defaultManagerSettings

  let lurl = url lopts ++ "/api/nowcasts?location=" ++ loc lopts

  request <- parseRequest lurl
  response <- httpLbs request manager

  let res = responseBody response
  let nowcastsDecoded = decode res :: Maybe [Nowcast]
  case nowcastsDecoded of
    Just nowcasts -> printTemp $ firstNowcast nowcasts
    Nothing -> print "Failed to decode response"

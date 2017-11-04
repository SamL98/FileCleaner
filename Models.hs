module Models where

data Date = Date { day :: Int,
                   month :: Int,
                   year :: Int } deriving (Show)

data Metadata = Metadata { name :: String,
                           creation :: Date,
                           change :: Date } deriving (Show)

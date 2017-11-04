module DateParser where
import Data.List.Split

data Date = Date { day :: Int,
                   month :: Int,
                   year :: Int } deriving (Show)

parseDate :: String -> Maybe Date
parseDate dateStr =
  let
    comps = filter (>0) $ map (read) $ splitOn "-" dateStr
  in
    case length comps of
      3 ->
        Just Date{ day=(comps!!2), month=(comps!!1), year=(comps!!0) }
      _ ->
        Nothing

module DateParser where
import Data.List.Split
--import Debug.Trace
import Models

dateComps :: String -> [Int]
dateComps str = filter (>0) $ map (read) $ splitOn "-" str

parseDate :: String -> Maybe Date
--parseDate dateStr | trace ("Date components: " ++ (show $ dateComps dateStr)) False = undefined
parseDate dateStr =
  let comps = dateComps dateStr in
    case length comps of
      3 ->
        Just Date{ day=(comps!!2), month=(comps!!1), year=(comps!!0) }
      _ ->
        Nothing

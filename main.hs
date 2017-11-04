module Main where
import System.Environment
import System.Process
import Data.List
import System.IO
import GHC.IO.Handle.Types
import Control.Monad
import Models
import DateParser
import Cleaner

nameKey = "kMDItemFSName"
createKey = "kMDItemFSCreationDate"
changeKey = "kMDItemFSContentChangeDate"

filterFiles :: [String] -> [String]
filterFiles files =
  filter (\file -> file !! 0 /= '#') files

filterEmpty :: String -> Bool
filterEmpty str =
  (str /= "(null)") && (str /= "")

findKey :: [(String, String)] -> String -> String
findKey pairs key =
  case lookup key pairs of
    Just value -> value
    Nothing -> ""

readMd :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO (Maybe Metadata)
readMd (Nothing, stdout, Nothing, ph) =
  case stdout of
    Just pipe -> do
      metadata <- hGetContents pipe
      let pairs = map (\x -> (x !! 0, x !! 2)) $ map (words) $ lines metadata
      let md = filter (filterEmpty) $ map (findKey pairs) [nameKey, createKey, changeKey]
      putStrLn $ show md
      case length md of
        3 ->
          let
            creation = parseDate $ md !! 1
            change = parseDate $ md !! 2
          in
            case (creation, change) of
              (Just crDate, Just chDate) ->
                return (Just Metadata{name=(md !! 0), creation=crDate, change=chDate})
              _ -> return (Nothing)
        _ ->
          return (Nothing)
    Nothing -> return (Nothing)

filterNone :: Maybe Metadata -> Bool
filterNone md =
  case md of
    Just _ -> True
    Nothing -> False

dispMd :: Maybe Metadata -> IO ()
dispMd md =
  case md of
    Just metadata -> putStrLn $ show metadata
    Nothing -> return ()

getMd :: String -> IO (Maybe Metadata)
getMd file =
  createProcess (proc "mdls" [file]){std_out = CreatePipe} >>= readMd

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> return ()
    _ -> do
      let dir = args !! 0
      files <- readProcess "ls" [dir] []
      todayStr <- readProcess "date" ["+%Y-%m-%d"] []
      case parseDate $ filter (/='\n') todayStr of
        Nothing -> return ()
        Just today -> do
          let filtered = map (\x -> dir++"/"++x) $ filterFiles $ lines files
          mapM (>>=dispMd) $ map (getMd) filtered
          putStrLn "Complete."

module Main where
import System.Exit
import System.Process
import Data.List
import System.IO
import GHC.IO.Handle.Types
import Control.Monad
import DateParser

data Metadata = Metadata { name :: String,
                           creation :: Maybe Date,
                           change :: Maybe Date } deriving (Show)

nameKey = "kMDItemFSName"
createKey = "kMDItemFSCreationDate"
changeKey = "kMDItemFSContentChangeDate"

filterFiles :: [String] -> [String]
filterFiles files =
  filter (\file -> file !! 0 /= '#') files

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
      let md = filter (/="(null)") $ map (findKey pairs) [nameKey, createKey, changeKey]
      case length md of
        3 ->
          return (Just Metadata { name=(md !! 0), creation=(parseDate $ md !! 1), change=(parseDate $ md !! 2) })
        _ ->
          return (Nothing)
    Nothing -> return (Nothing)

dispMd :: Maybe Metadata -> IO ()
dispMd md =
  case md of
    Just metadata -> putStrLn $ show metadata
    Nothing -> return ()

main :: IO ()
main = do
    files <- readProcess "ls" [] []
    let filtered = filterFiles $ lines files
    mapM (>>=dispMd) $ map ( \x ->
            createProcess (proc "mdls" [x]){std_out = CreatePipe}
            >>= readMd
        ) $ filtered
    putStrLn "Complete."
     

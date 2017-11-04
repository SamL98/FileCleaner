import System.Exit
import System.Process
import Data.List
import Data.List.Split
import System.IO
import GHC.IO.Handle.Types
import Control.Monad

data Metadata = Metadata { name :: String,
                           creation :: String,
                           change :: String } deriving (Show)

mdKeys :: [String]
mdKeys = ["kMDItemFSContentChangeDate", "kMDItemFSCreationDate", "kMDItemFSName"]

filterFiles :: [String] -> [String]
filterFiles files =
  filter (\file -> file !! 0 /= '#') files

findKey :: [String] -> [String] -> String -> String
findKey keys vals key =
  case elemIndex key keys of
    Just index -> vals !! index
    Nothing -> ""

readMd :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO (Maybe Metadata)
readMd (Nothing, stdout, Nothing, ph) =
  case stdout of
    Just pipe -> do
      metadata <- hGetContents pipe
      let pairs = map (\x -> (x !! 0, x !! 2)) $ map (words) $ lines metadata
      let keys = map (fst) pairs
      let vals = map (snd) pairs
      let md = map (findKey keys vals) $ reverse mdKeys
      return (Just Metadata { name=(md !! 0), creation=(md !! 1), change=(md !! 2) })
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
     

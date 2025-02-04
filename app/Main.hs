-- import qualified Rexp
-- import qualified EdelmannZipper
-- import Lexer
-- import System.IO

-- main :: IO ()
-- main = do
--     putStrLn "Enter the filename (without path):"
--     filename <- getLine
--     let filePath = "src/examples/" ++ filename
--     fileContent <- readFile filePath
--     print fileContent
--     let tokens = tokenise fileContent
--     print tokens



import Control.Monad (forM_)
import Data.IORef
import qualified Data.HashMap.Strict as HM

type Key = (Char, Int)
type Value = [Int]
type MutableMap = IORef (HM.HashMap Key Value)

-- Initialize an empty mutable map
newMutableMap :: IO MutableMap
newMutableMap = newIORef HM.empty

-- Insert or update a key in the map
insertOrUpdate :: MutableMap -> Key -> Value -> IO ()
insertOrUpdate ref key value = do
    hashmap <- readIORef ref
    let updatedMap = HM.insert key value hashmap
    writeIORef ref updatedMap

-- Retrieve a value from the map
lookupValue :: MutableMap -> Key -> IO (Maybe Value)
lookupValue ref key = do
    hashmap <- readIORef ref
    return $ HM.lookup key hashmap

-- Example usage
main :: IO ()
main = do
    mapRef <- newMutableMap
    insertOrUpdate mapRef ('a', 1) [10, 20, 30]
    insertOrUpdate mapRef ('b', 2) [40, 50]

    -- Retrieve values
    val1 <- lookupValue mapRef ('a', 1)
    val2 <- lookupValue mapRef ('b', 2)

    print val1  -- Should print: Just [10,20,30]
    print val2  -- Should print: Just [40,50]
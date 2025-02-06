import qualified RexpZipper
import System.IO

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    print fileContent
    let tokens = RexpZipper.tokenise fileContent
    print tokens




import qualified Rexp
import qualified EdelmannZipper
import Lexer
import System.IO

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    print fileContent
    let tokens = tokenise fileContent
    print tokens




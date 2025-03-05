import ZipperLexer
import RexpZipperv2
import System.IO

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    print fileContent
    -- ws <- whileRegs
    tokens <- ZipperLexer.tokenise fileContent
    -- es <- run fileContent ws
    print tokens



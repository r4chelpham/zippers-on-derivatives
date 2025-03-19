import ZipperLexer
import RexpZipperv2
import RexpZipper

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    putStrLn fileContent
    let tokens = RexpZipper.tokenise fileContent
    -- es <- run fileContent ws
    print tokens



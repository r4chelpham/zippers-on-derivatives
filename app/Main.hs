import ZipperLexer(whileRegs)
import RexpZipperv2
import System.IO

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    print fileContent
    -- tokens <- ZipperLexer.tokenise fileContent
    ws <- whileRegs
    es <- run fileContent ws
    print es




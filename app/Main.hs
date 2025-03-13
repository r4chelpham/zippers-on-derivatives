import ZipperLexer
import RexpZipperv2

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    putStrLn fileContent
    -- ws <- whileRegs
    -- tokens <- ZipperLexer.tokenise fileContent
    -- es <- run fileContent ws
    -- print tokens



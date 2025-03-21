import ZipperLexer

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    putStrLn fileContent
    let tokens = ZipperLexer.tokenise fileContent
    -- es <- run fileContent ws
    print tokens



import qualified EdelmannZipper as Z

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    putStrLn fileContent
    result <- Z.tokenise (Z.build Z.rules) fileContent
    print result




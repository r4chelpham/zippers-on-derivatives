import ZipperLexerv2
import GHC.IORef
import RexpZipperv2

main :: IO ()
main = do
    putStrLn "Enter the filename (without path):"
    filename <- getLine
    let filePath = "src/examples/" ++ filename
    fileContent <- readFile filePath
    putStrLn fileContent
    tokens <- ZipperLexerv2.tokenise fileContent
    -- es <- run fileContent ws
    print tokens
    c <- readIORef RexpZipperv2.count
    print c
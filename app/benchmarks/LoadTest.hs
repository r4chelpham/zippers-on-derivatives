import Benchmark
import Lexer
import ZipperLexer
import ZipperLexerv2
import EdelmannLexer
import Control.Monad (forM_)

main :: IO ()
main = do
    contents <- readFile "src/examples/fib.while"
    let repetitions = [4, 8, 16, 32, 64]
        repeatedContents = [(n, concat (replicate n contents)) | n <- repetitions]
    
    forM_ repeatedContents $ \(n, repeated) -> do
        putStrLn $ "Benchmarking with " ++ show (12*n) ++ " lines"
        putStrLn $ "Urban: "
        benchmarkLex "tokenise" Lexer.tokenise ["test_" ++ show n ++ ".while"] [repeated]
        putStrLn $ "Edelmann: "
        benchmarkLexIO "tokenise" EdelmannLexer.tokenise ["test_" ++ show n ++ ".while"] [repeated]
        putStrLn $ "RexpZipper: "
        benchmarkLex "tokenise" ZipperLexer.tokenise ["test_" ++ show n ++ ".while"] [repeated]
        putStrLn $ "RexpZipperv2: "
        benchmarkLexIO "tokenise" ZipperLexerv2.tokenise ["test_" ++ show n ++ ".while"] [repeated]

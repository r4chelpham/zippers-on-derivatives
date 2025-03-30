import Benchmark
import Lexer
import ZipperLexer
import ZipperLexerv2
import EdelmannLexer

main :: IO ()
main = do
    contents <- readFile "src/examples/test.while"
    benchmark "tokenise" Lexer.tokenise ["test.while"] [contents]
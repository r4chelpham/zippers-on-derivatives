import Benchmark
import qualified EdelmannLexer as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmarkLexIO "tokenise" L.tokenise testFiles contents

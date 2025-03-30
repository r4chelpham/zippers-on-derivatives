import Benchmark
import qualified EdelmannLexer as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmarkIO "tokenise" L.tokenise testFiles contents

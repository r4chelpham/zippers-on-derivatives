import Benchmark
import qualified ZipperLexer as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmarkLex "tokenise" L.tokenise testFiles contents

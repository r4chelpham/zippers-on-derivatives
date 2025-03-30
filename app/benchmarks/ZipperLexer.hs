import Benchmark
import qualified ZipperLexer as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmark "tokenise" L.tokenise testFiles contents

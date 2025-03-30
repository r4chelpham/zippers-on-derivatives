import Benchmark
import qualified ZipperLexerv2 as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmarkIO "tokenise" L.tokenise testFiles contents

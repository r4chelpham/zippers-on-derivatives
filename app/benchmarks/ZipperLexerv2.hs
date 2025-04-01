import Benchmark
import qualified ZipperLexerv2 as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmarkLexIO "tokenise" L.tokenise testFiles contents

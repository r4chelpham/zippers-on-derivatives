import Benchmark
import qualified Lexer as L

main :: IO ()
main = do
  contents <- loadFiles testFiles
  benchmark "tokenise" L.tokenise testFiles contents

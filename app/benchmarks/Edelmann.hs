import Criterion.Main
import qualified EdelmannLexer as L

main :: IO ()
main = do
  let basePath = "src/examples/"
      testFiles = 
        [ "collatz.while"
        , "fib.while"
        , "collatz2.while"
        , "factors.while"
        , "loops.while"
        , "primes.while"
        ]

  fileContents <- mapM (\f -> readFile (basePath ++ f)) testFiles

  let benchmarks = zipWith (\file content ->
                      bench ("tokenise " ++ file) $ nfIO (L.tokenise content)
                    ) testFiles fileContents

  defaultMain benchmarks

import Criterion.Main
import qualified RexpZipperv2 as Z

main :: IO ()
main = do
    let lengths = [5, 10, 20, 25, 50, 100, 1000, 10000]
    let strings = map (`replicate` 'a') (lengths ++ [100000])

    let makeBenchmark s n = bench ("Match (a*)*b, n = " ++ show n) $
            nfIO $ do
            regex <- ("a" Z.*> ()) Z.*> () Z.<~> "b"
            Z.matcher s regex

    let benchmarks = zipWith makeBenchmark strings (lengths ++ [100000])
    defaultMain benchmarks

    let lengths = [5, 10, 20, 25, 50, 100, 1000]

    let rexpGen n = ("a" Z.?> ()) Z.^> n Z.<~> ("a" Z.^> n)

    let makeBenchmark n = bench ("Match (a?)^n ~ a^n, n = " ++ show n) $
            nfIO $  do
            regex <- rexpGen n
            Z.matcher (replicate n 'a') regex

    defaultMain (map makeBenchmark lengths)
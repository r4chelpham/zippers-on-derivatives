import Criterion.Main
import Benchmark
import qualified RexpZipper as Z

main :: IO ()
main = do
    let lengths = [5, 10, 20, 25, 50, 100, 1000, 10000]
    let r = ("a" Z.*> ()) Z.*> () Z.<~> "b"
    let benchmarks = zipWith (\s n ->
                    bench ("Match (a*)*b, n = " ++ show n) $ nf (`Z.matcher` r) s
                  ) (strings (lengths ++ [100000])) (lengths ++ [100000])
    defaultMain benchmarks
    let rexpGen n = ("a" Z.?> ()) Z.^> n Z.<~> ("a" Z.^> n)
    let benchmarks = zipWith (\s n ->
                      bench ("Match (a?)^n ~ a^n, n = " ++ show n) $ nf (`Z.matcher` (rexpGen n)) s
                    ) (strings lengths) lengths
    defaultMain benchmarks
import Benchmark
import qualified EdelmannZipper as Z
import qualified Rexp as R

main :: IO ()
main = do
    let lengths = [5, 10, 20, 25, 50, 100, 1000]
    let r1 = ("a" R.*> ()) R.*> () R.<~> "b"
    benchmarkMatch "Match (a*)*b, n = " r1 Z.matcher (lengths ++ [10000, 100000])
    benchmarkMatchNTimes "Match (a?)^n ~ a^n, n = " (\n -> ("a" R.?> ()) R.^> n R.<~> ("a" R.^> n)) Z.matcher lengths 
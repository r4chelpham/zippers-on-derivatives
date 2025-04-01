module Benchmark where

import Criterion.Main
import Token
import Rexp

basePath :: [Char]
basePath = "src/examples/"

testFiles :: [[Char]]
testFiles =
        [ "collatz.while"
        , "fib.while"
        , "collatz2.while"
        , "factors.while"
        , "loops.while"
        , "primes.while"
        ]

strings :: [Int] -> [[Char]]
strings = map (`replicate` 'a')

loadFiles :: [String] -> IO [String]
loadFiles = mapM (\f -> readFile (basePath ++ f))

benchmarkLexIO :: String -> (String -> IO [Token]) -> [String] -> [String] -> IO ()
benchmarkLexIO fName f files contents = do
  let benchmarks = zipWith (\file content ->
                      bench (fName ++ " " ++ file) $ nfIO (f content)
                    ) files contents
  defaultMain benchmarks

benchmarkLex :: String -> (String -> [Token]) -> [String] -> [String] -> IO ()
benchmarkLex fName f files contents = do
  let benchmarks = zipWith (\file content ->
                      bench (fName ++ ": " ++ file) $ nf f content
                    ) files contents
  defaultMain benchmarks

benchmarkMatch :: [Char] -> Rexp -> (Rexp -> String -> Bool) -> [Int] -> IO ()
benchmarkMatch text r f lengths = do
  let benchmarks = zipWith (\s n ->
                    bench (text ++ show n) $ nf (f r) s
                  ) (strings lengths) lengths
  defaultMain benchmarks

benchmarkMatchNTimes :: [Char] -> (Int -> Rexp) -> (Rexp -> String -> Bool) -> [Int] -> IO ()
benchmarkMatchNTimes text rexpGen f lengths = do
  let benchmarks = zipWith (\s n ->
                      bench (text ++ show n) $ nf (f (rexpGen n)) s
                    ) (strings lengths) lengths
  defaultMain benchmarks

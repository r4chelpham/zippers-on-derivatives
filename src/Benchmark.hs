module Benchmark where

import Criterion.Main
import Token

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

loadFiles :: [String] -> IO [String]
loadFiles = mapM (\f -> readFile (basePath ++ f))

benchmarkIO :: String -> (String -> IO [Token]) -> [String] -> [String] -> IO ()
benchmarkIO fName f files contents = do
  let benchmarks = zipWith (\file content ->
                      bench (fName ++ " " ++ file) $ nfIO (f content)
                    ) files contents
  defaultMain benchmarks

benchmark :: String -> (String -> [Token]) -> [String] -> [String] -> IO ()
benchmark fName f files contents = do
  let benchmarks = zipWith (\file content ->
                      bench (fName ++ ": " ++ file) $ nf f content
                    ) files contents
  defaultMain benchmarks

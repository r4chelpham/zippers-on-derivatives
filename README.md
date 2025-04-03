# zippers-on-derivatives

This is the repository for the artifact associated with Zippers on Derivatives, a final year undergraduate dissertation.

This artifact includes 3 implementations:
- Matching and lexing with zippers from Efficient Parsing with Derivatives and Zippers (Edelmann)
- Matching and lexing with zippers using the method from Parsing with Zippers (A Functional Pearl) (Darragh and Adams)
- Matching and lexing with derivatives (Urban, Sulzmann and Lu)

## Details
Creator: Rachel Pham

Module: 6CCS3PRJ

## User Guide
This is a user guide on setting up the project and running it.

### Prerequisites
#### Stack
The project uses Stack (version 3.3.1) for managing dependencies and building the Haskell project.
#### GHC
The project requires GHC (Glasgow Haskell Compiler) version 8.8.4.
#### WSL (Optional, for Windows users)
This project requires WSL version 2 (if you are using Windows)

### Instructions
1. Clone the repository
```
git clone <this repository link>
```
2. Install Stack
```
sudo apt install haskell-stack
```
3. Pull from main
```
git pull origin main
``` 
4. Run the following commands
```
stack upgrade --force-download
stack clean
stack setup
stack build
```
This will take a while because it is downloading all the dependencies specified in the `zippers-on-derivatives.cabal` file.

### Running the project
#### Benchmarks
The `benchmarks` directory contains a set of benchmarks to evaluate and test matching and lexing.

You can benchmark the un-memoised and memoised versions of our matcher and lexer with the alternative implementations by Urban and Edelmann.

To run all benchmarks, run:
```
stack bench
```

The names of the benchmarks can be found using this command:
```
stack ide targets
> ... anything with the prefix zippers-on-derivatives:bench: is a benchmark
```

If you want to run a specific benchmark, you can use this command:
```
stack bench <name of benchmark>
```
e.g. you can run the benchmark `rexpzipper-matching` by doing:
```
stack bench zippers-on-derivatives:bench:rexpzipper-matching
```

#### Main function
The `app/Main.hs` file is the main file of the program. It contains an example program where, given the file name as input from the terminal, `ZipperLexerv2` is used to lex a program from the `While` language.

You can execute this by running the command:
```
stack run
```

#### Tests
The `tests` directory contains tests that were used to validate the implementation during development.

You can run the tests by using this command:
```
stack test
```

#### Experimenting
If you want to have a go at using the matcher and lexer yourself, you can do so in GHCi, Haskell's interactive environment.
Just run
```
stack ghci
```
and import the modules as `qualified`, e.g.
```
import qualified RexpZipperv2 as Z
```

Then, you can use the DSL specified for each module to create your own regexes, e.g.
```
let r = "a" Z.*> () Z.*> () Z.<~> "b" -- | Creates (a*)*b 
```

Because `RexpZipperv2` works in the `IO` monad, the notation for creating regexes and calling functions is different:
```
r <- "a" Z.*> () Z.*> () Z.<~> "b"
Z.matcher r (replicate 1000 'a')
```
Furthermore, because memories can only be used once, you need to create the regex again in order to use it.
```
r <- "a" Z.*> () Z.*> () Z.<~> "b"
Z.matcher r "aaab"
> -- | returns a successful result
Z.matcher r "aaab"
> [] -- | returns no output because the regex's memory has been permanently altered 
```

{-# LANGUAGE ScopedTypeVariables #-}
module ZipperLexerSpec where
import Test.QuickCheck
import Test.Hspec
import qualified ZipperLexer as ZL
import Lexer
import Token
import Control.Monad (forM_)

spec :: Spec
spec = do
    describe "Lexing keyword tests" $ do
        it "lexs keyword 'while' " $ do
            let res = ZL.tokenise  "while"
            res `shouldBe` [T_KEYWORD "while"]

        it "lexs keyword 'if' " $ do
            let res = ZL.tokenise  "if"
            res `shouldBe` [T_KEYWORD "if"]

        it "lexs keyword 'then' " $ do
            let res = ZL.tokenise  "then"
            res `shouldBe` [T_KEYWORD "then"]

        it "lexs keyword 'else' " $ do
            let res = ZL.tokenise  "else"
            res `shouldBe` [T_KEYWORD "else"]

        it "lexs keyword 'do' " $ do
            let res = ZL.tokenise  "do"
            res `shouldBe` [T_KEYWORD "do"]

        it "lexs keyword 'for' " $ do
            let res = ZL.tokenise  "for"
            res `shouldBe` [T_KEYWORD "for"]

        it "lexs keyword 'to' " $ do
            let res = ZL.tokenise  "to"
            res `shouldBe` [T_KEYWORD "to"]

        it "lexs keyword 'true' " $ do
            let res = ZL.tokenise  "true"
            res `shouldBe` [T_KEYWORD "true"]

        it "lexs keyword 'false' " $ do
            let res = ZL.tokenise  "false"
            res `shouldBe` [T_KEYWORD "false"]

        it "lexs keyword 'read' " $ do
            let res = ZL.tokenise  "read"
            res `shouldBe` [T_KEYWORD "read"]

        it "lexs keyword 'write' " $ do
            let res = ZL.tokenise  "write"
            res `shouldBe` [T_KEYWORD "write"]

        it "lexs keyword 'skip' " $ do
            let res = ZL.tokenise  "skip"
            res `shouldBe` [T_KEYWORD "skip"]

        it "lexs keyword 'break' " $ do
            let res = ZL.tokenise  "break"
            res `shouldBe` [T_KEYWORD "break"]

        it "does not lex invalid keyword 'whil' " $ do
            let res = ZL.tokenise  "whil"
            res `shouldNotBe` [T_KEYWORD "while"]

        it "does not lex invalid keyword 'iff' " $ do
            let res = ZL.tokenise  "iff"
            res `shouldNotBe` [T_KEYWORD "if"]

        it "does not lex invalid keyword 'then1' " $ do
            let res = ZL.tokenise  "then1"
            res `shouldNotBe` [T_KEYWORD "then"]

        it "does not lex invalid keyword 'Else' (capitalised)" $ do
            let res = ZL.tokenise  "Else"
            res `shouldNotBe` [T_KEYWORD "else"]

        it "does not lex invalid keyword 'd o' (with space)" $ do
            let res = ZL.tokenise  "d o"
            res `shouldNotBe` [T_KEYWORD "do"]

        it "does not lex invalid keyword 'forto' (concatenated)" $ do
            let res = ZL.tokenise  "forto"
            res `shouldNotBe` [T_KEYWORD "for"]

        it "does not lex invalid keyword 'tru' (substring of true)" $ do
            let res = ZL.tokenise  "tru"
            res `shouldNotBe` [T_KEYWORD "true"]

        it "does not lex invalid keyword 'False' (capitalised)" $ do
            let res = ZL.tokenise  "False"
            res `shouldNotBe` [T_KEYWORD "false"]

        it "does not lex invalid keyword 'ReadWrite' (merged keywords)" $ do
            let res = ZL.tokenise  "ReadWrite"
            res `shouldNotBe` [T_KEYWORD "read"]

        it "does not lex invalid keyword 'skip!' (special char)" $ do
            let res = ZL.tokenise  "skip!"
            res `shouldBe` []

        it "does not lex invalid keyword 'b_r_e_a_k' (with underscores)" $ do
            let res = ZL.tokenise  "b_r_e_a_k"
            res `shouldNotBe` [T_KEYWORD "break"]

    describe "Lexing numbers tests" $ do
        it "lexs numbers" $ property $
            \(Positive n :: Positive Int) -> do
                let n' = show n
                let res = ZL.tokenise  n'
                res `shouldBe` [T_NUM n']

        it "lexs number '0'" $ do
            let res = ZL.tokenise  "0"
            res `shouldBe` [T_NUM "0"]

        it "lexs numbers with leading whitespace '  56'" $ do
            let res = ZL.tokenise  "  56"
            res `shouldBe` [T_NUM "56"]

        -- | Invalid cases.
        it "does not lex decimals '3.14'" $ do
            let res = ZL.tokenise  "3.14"
            res `shouldBe` []

        it "does not lex invalid number '12abc'" $ do
            let res = ZL.tokenise  "12abc"
            res `shouldNotBe` [T_NUM "12"]

        it "does not lex invalid number '42!'" $ do
            let res = ZL.tokenise  "42!"
            res `shouldBe` []

        it "does not lex negative numbers" $ property $
            \(Positive n :: Positive Int) -> do
                let n' = show (-n)
                let res = ZL.tokenise  n'
                res `shouldNotBe` [T_NUM n']

        it "does not lex numbers with leading 0s" $ property $
            \(n :: Int) -> do
                let n' = "00" ++ show n
                let res = ZL.tokenise  n'
                res `shouldNotBe` [T_NUM n']

    describe "Lexing identifiers tests" $ do
        it "lexes valid strings as identifiers" $ property $
            forAll (listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])) $ \s -> do
                let res = ZL.tokenise  s
                res `shouldBe` [T_ID s]

        it "lexes strings containing keywords as identifiers (longest match)" $ do
            let res = ZL.tokenise "fifoo"
            res `shouldBe` [T_ID "fifoo"]
            let res' = ZL.tokenise "iffoo"
            res' `shouldBe` [T_ID "iffoo"]

        it "lexes strings containing keywords as identifiers (longest match)" $ do
            let res = ZL.tokenise "whilee"
            res `shouldBe` [T_ID "whilee"]

        it "lexes valid strings (containing numbers and underscores) as identifiers" $ property $
            forAll (
                (,) <$>
                elements (['a'..'z'] ++ ['A'..'Z']) <*>
                listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")))
            $ \(c, s) -> do
                let identifier = c : s
                let res = ZL.tokenise identifier
                res `shouldBe` [T_ID identifier]

        it "does not lex underscores as identifiers" $ do
            let res = ZL.tokenise  "_"
            res `shouldBe` []

        it "" $ do
            let res = ZL.tokenise  "read n;"
            res `shouldBe` [T_KEYWORD "read", T_ID "n", T_SEMI]

        it "does not lex strings starting with numbers as identifiers" $ property $
            forAll ((,) <$> elements ['0'..'9'] <*> listOf1  (elements (['a'..'z'] ++ ['A'..'Z'] ++ "_"))) $ \(c, s) -> do
                let invalidIdentifier = c : s
                let res = ZL.tokenise invalidIdentifier
                res `shouldNotBe` [T_ID invalidIdentifier]

        it "does not lex strings starting with underscores as identifiers" $ property $
            forAll ((,) '_' <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))) $ \(c, s) -> do
                let invalidIdentifier = c : s
                let res = ZL.tokenise  invalidIdentifier
                res `shouldBe` []

    describe "While language lexing tests" $ do
        describe "Lexer file tests" $ do
            forM_ testFiles $ \filePath -> do
                it ("lexes " ++ filePath) $ do
                    fileContent <- readFile ("src/examples/" ++ filePath)
                    let expected = Lexer.tokenise fileContent
                    let res = ZL.tokenise fileContent
                    res `shouldBe` expected
        where
            testFiles :: [FilePath]
            testFiles = 
                [ "collatz.while"
                , "fib.while"
                , "collatz2.while"
                , "factors.while"
                , "loops.while"
                , "primes.while"
                ]
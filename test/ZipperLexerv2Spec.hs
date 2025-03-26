{-# LANGUAGE ScopedTypeVariables #-}
module ZipperLexerv2Spec where
import Test.QuickCheck
import Test.Hspec
import qualified ZipperLexerv2 as ZL
import Token
import Control.Monad(forM_)
import Lexer


spec :: Spec
spec = do
    describe "Lexing keyword tests" $ do
        it "lexs keyword 'while' " $ do
            res <- ZL.tokenise "while"
            res `shouldBe` [T_KEYWORD "while"]
        
        it "lexs keyword 'if' " $ do
            res <- ZL.tokenise "if"
            res `shouldBe` [T_KEYWORD "if"]
        
        it "lexs keyword 'then' " $ do
            res <- ZL.tokenise "then"
            res `shouldBe` [T_KEYWORD "then"]
        
        it "lexs keyword 'else' " $ do
            res <- ZL.tokenise "else"
            res `shouldBe` [T_KEYWORD "else"]
        
        it "lexs keyword 'do' " $ do
            res <- ZL.tokenise "do"
            res `shouldBe` [T_KEYWORD "do"]
        
        it "lexs keyword 'for' " $ do
            res <- ZL.tokenise "for"
            res `shouldBe` [T_KEYWORD "for"]
        
        it "lexs keyword 'to' " $ do
            res <- ZL.tokenise "to"
            res `shouldBe` [T_KEYWORD "to"]
        
        it "lexs keyword 'true' " $ do
            res <- ZL.tokenise "true"
            res `shouldBe` [T_KEYWORD "true"]
        
        it "lexs keyword 'false' " $ do
            res <- ZL.tokenise "false"
            res `shouldBe` [T_KEYWORD "false"]
        
        it "lexs keyword 'read' " $ do
            res <- ZL.tokenise "read"
            res `shouldBe` [T_KEYWORD "read"]
        
        it "lexs keyword 'write' " $ do
            res <- ZL.tokenise "write"
            res `shouldBe` [T_KEYWORD "write"]
        
        it "lexs keyword 'skip' " $ do
            res <- ZL.tokenise "skip"
            res `shouldBe` [T_KEYWORD "skip"]
        
        it "lexs keyword 'break' " $ do
            res <- ZL.tokenise "break"
            res `shouldBe` [T_KEYWORD "break"]

        it "does not lex invalid keyword 'whil' " $ do
            res <- ZL.tokenise "whil"
            res `shouldNotBe` [T_KEYWORD "while"]

        it "does not lex invalid keyword 'iff' " $ do
            res <- ZL.tokenise "iff"
            res `shouldNotBe` [T_KEYWORD "if"]

        it "does not lex invalid keyword 'then1' " $ do
            res <- ZL.tokenise "then1"
            res `shouldNotBe` [T_KEYWORD "then"]

        it "does not lex invalid keyword 'Else' (capitalised)" $ do
            res <- ZL.tokenise "Else"
            res `shouldNotBe` [T_KEYWORD "else"]

        it "does not lex invalid keyword 'd o' (with space)" $ do
            res <- ZL.tokenise "d o"
            res `shouldNotBe` [T_KEYWORD "do"]

        it "does not lex invalid keyword 'forto' (concatenated)" $ do
            res <- ZL.tokenise "forto"
            res `shouldNotBe` [T_KEYWORD "for"]

        it "does not lex invalid keyword 'tru' (substring of true)" $ do
            res <- ZL.tokenise "tru"
            res `shouldNotBe` [T_KEYWORD "true"]

        it "does not lex invalid keyword 'False' (capitalised)" $ do
            res <- ZL.tokenise "False"
            res `shouldNotBe` [T_KEYWORD "false"]

        it "does not lex invalid keyword 'ReadWrite' (merged keywords)" $ do
            res <- ZL.tokenise "ReadWrite"
            res `shouldNotBe` [T_KEYWORD "read"]

        it "does not lex invalid keyword 'skip!' (special char)" $ do
            res <- ZL.tokenise "skip!"
            res `shouldNotBe` [T_KEYWORD "skip"]

        it "does not lex invalid keyword 'b_r_e_a_k' (with underscores)" $ do
            res <- ZL.tokenise "b_r_e_a_k"
            res `shouldNotBe` [T_KEYWORD "break"]

    describe "Lexing numbers tests" $ do
        it "lexs numbers" $ property $
            \(Positive n :: Positive Int) -> do 
                let n' = show n
                res <- ZL.tokenise n'
                res `shouldBe` [T_NUM n']

        it "lexs number '0'" $ do
            res <- ZL.tokenise "0"
            res `shouldBe` [T_NUM "0"]

        it "lexs numbers with leading whitespace '  56'" $ do
            res <- ZL.tokenise "  56"
            res `shouldBe` [T_NUM "56"]

        -- | Invalid cases.
        it "does not lex decimals '3.14'" $ do
            res <- ZL.tokenise "3.14"
            res `shouldNotBe` [T_NUM "3"]

        it "does not lex invalid number '12abc'" $ do
            res <- ZL.tokenise "12abc"
            res `shouldNotBe` [T_NUM "12"]

        it "does not lex invalid number '42!'" $ do
            res <- ZL.tokenise "42!"
            res `shouldNotBe` [T_NUM "42"]

        it "does not lex negative numbers" $ property $
            \(Positive n :: Positive Int) -> do
                let n' = show (-n)
                res <- ZL.tokenise n'
                res `shouldNotBe` [T_NUM n']

        it "does not lex numbers with leading 0s" $ property $
            \(n :: Int) -> do
                let n' = "00" ++ show n
                res <- ZL.tokenise n'
                res `shouldNotBe` [T_NUM n']

    describe "Lexing identifiers tests" $ do
        it "lexes valid strings as identifiers" $ property $
            forAll (listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])) $ \s -> do
                res <- ZL.tokenise s
                res `shouldBe` [T_ID s]

        it "lexes valid strings (containing numbers and underscores) as identifiers" $ property $
            forAll (
                (,) <$> 
                elements (['a'..'z'] ++ ['A'..'Z']) <*> 
                listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))) 
            $ \(c, s) -> do
                let identifier = c : s
                res <- ZL.tokenise identifier
                res `shouldBe` [T_ID identifier]
                
        it "does not lex underscores as identifiers" $ do
            res <- ZL.tokenise "_"
            res `shouldNotBe` [T_ID "_"]

        it "" $ do
            res <- ZL.tokenise "read n;"
            res `shouldBe` [T_KEYWORD "read", T_ID "n", T_SEMI]

    describe "While language lexing tests" $ do
        describe "Lexer file tests" $ do
            forM_ testFiles $ \filePath -> do
                it ("lexes " ++ filePath) $ do
                    fileContent <- readFile ("src/examples/" ++ filePath)
                    let expected = Lexer.tokenise fileContent
                    res <- ZL.tokenise fileContent
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

        -- it "does not lex strings starting with numbers as identifiers" $ property $
        --     forAll ((,) <$> elements ['0'..'9'] <*> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))) $ \(c, s) -> do
        --         let invalidIdentifier = c : s
        --         res <- ZL.tokenise invalidIdentifier
        --         res `shouldNotBe` [T_ID invalidIdentifier]

        -- it "does not lex strings starting with underscores as identifiers" $ property $
        --     forAll ((,) <$> pure '_' <*> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))) $ \(c, s) -> do
        --         let invalidIdentifier = c : s
        --         res <- ZL.tokenise invalidIdentifier
        --         res `shouldNotBe` [T_ID invalidIdentifier]

        
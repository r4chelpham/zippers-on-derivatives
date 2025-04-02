{-# LANGUAGE ScopedTypeVariables #-}

module EdelmannLexerSpec where
    
import Test.QuickCheck
import Test.Hspec
import EdelmannLexer
import Lexer
import Control.Monad(forM_)
import Token

spec :: Spec
spec = do 
    describe "Lexing While registers tests" $ do 
        it "matches keyword 'while' " $ do
            res <- EdelmannLexer.tokenise "while"
            res `shouldBe` [T_KEYWORD "while"]
        
        it "matches keyword 'if' " $ do
            res <- EdelmannLexer.tokenise "if"
            res `shouldBe` [T_KEYWORD "if"]
        
        it "matches keyword 'then' " $ do
            res <- EdelmannLexer.tokenise "then"
            res `shouldBe` [T_KEYWORD "then"]
        
        it "matches keyword 'else' " $ do
            res <- EdelmannLexer.tokenise "else"
            res `shouldBe` [T_KEYWORD "else"]
        
        it "matches keyword 'do' " $ do
            res <- EdelmannLexer.tokenise "do"
            res `shouldBe` [T_KEYWORD "do"]
        
        it "matches keyword 'for' " $ do
            res <- EdelmannLexer.tokenise "for"
            res `shouldBe` [T_KEYWORD "for"]
        
        it "matches keyword 'to' " $ do
            res <- EdelmannLexer.tokenise "to"
            res `shouldBe` [T_KEYWORD "to"]
        
        it "matches keyword 'true' " $ do
            res <- EdelmannLexer.tokenise "true"
            res `shouldBe` [T_KEYWORD "true"]
        
        it "matches keyword 'false' " $ do
            res <- EdelmannLexer.tokenise "false"
            res `shouldBe` [T_KEYWORD "false"]
        
        it "matches keyword 'read' " $ do
            res <- EdelmannLexer.tokenise "read"
            res `shouldBe` [T_KEYWORD "read"]
        
        it "matches keyword 'write' " $ do
            res <- EdelmannLexer.tokenise "write"
            res `shouldBe` [T_KEYWORD "write"]
        
        it "matches keyword 'skip' " $ do
            res <- EdelmannLexer.tokenise "skip"
            res `shouldBe` [T_KEYWORD "skip"]
        
        it "matches keyword 'break' " $ do
            res <- EdelmannLexer.tokenise "break"
            res `shouldBe` [T_KEYWORD "break"]

        it "does not match invalid keyword 'whil' " $ do
            res <- EdelmannLexer.tokenise "whil"
            res `shouldNotBe` [T_ID "while"]

        it "does not match invalid keyword 'iff' " $ do
            res <- EdelmannLexer.tokenise "iff"
            res `shouldNotBe` [T_KEYWORD "if"]

        it "does not match invalid keyword 'then1' " $ do
            res <- EdelmannLexer.tokenise "then1"
            res `shouldNotBe` [T_KEYWORD "then"]

        it "does not match invalid keyword 'Else' (capitalized)" $ do
            res <- EdelmannLexer.tokenise "Else"
            res `shouldNotBe` [T_KEYWORD "else"]

        it "does not match invalid keyword 'd o' (with space)" $ do
            res <- EdelmannLexer.tokenise "d o"
            res `shouldNotBe` [T_KEYWORD "do"]

        it "does not match invalid keyword 'forto' (concatenated)" $ do
            res <- EdelmannLexer.tokenise "forto"
            res `shouldNotBe` [T_KEYWORD "for"]

        it "does not match invalid keyword 'tru' (substring of true)" $ do
            res <- EdelmannLexer.tokenise "tru"
            res `shouldNotBe` [T_KEYWORD "true"]

        it "does not match invalid keyword 'False' (capitalized)" $ do
            res <- EdelmannLexer.tokenise "False"
            res `shouldNotBe` [T_KEYWORD "false"]

        it "does not match invalid keyword 'ReadWrite' (merged keywords)" $ do
            res <- EdelmannLexer.tokenise "ReadWrite"
            res `shouldNotBe` [T_KEYWORD "read"]

        it "does not match invalid keyword 'skip!' (special char)" $ do
            res <- EdelmannLexer.tokenise "skip!"
            res `shouldNotBe` [T_KEYWORD "skip"]

        it "does not match invalid keyword 'b_r_e_a_k' (with underscores)" $ do
            res <- EdelmannLexer.tokenise "b_r_e_a_k"
            res `shouldNotBe` [T_KEYWORD "break"]

        it "lexes valid strings as identifiers" $ property $
            forAll (listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'])) $ \cs -> do
                res <- EdelmannLexer.tokenise cs
                res `shouldBe` [T_ID cs]

        it "does not lex strings starting with numbers as identifiers" $ property $
            forAll ((,) <$> elements ['0'..'9'] <*> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))) $ \(c, s) -> do
                let invalidIdentifier = c : s
                res <- EdelmannLexer.tokenise invalidIdentifier
                res `shouldNotBe` [T_ID invalidIdentifier]

        it "does not lex strings starting with underscores as identifiers" $ property $
            forAll ((,) <$> pure '_' <*> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"))) $ \(c, s) -> do
                let invalidIdentifier = c : s
                res <- EdelmannLexer.tokenise invalidIdentifier
                res `shouldNotBe` [T_ID invalidIdentifier]

        it "lexes valid strings" $ property $
            forAll (listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "() \t\n")) $ \cs -> do
                let s = "\"" ++ cs ++ "\""
                res <- EdelmannLexer.tokenise s
                res `shouldBe` [T_STRING s]
            

    describe "Lexer file tests" $ do
        forM_ testFiles $ \filePath -> do
            it ("lexes " ++ filePath) $ do
                fileContent <- readFile ("src/examples/" ++ filePath)
                let expected = Lexer.tokenise fileContent
                res <- EdelmannLexer.tokenise fileContent
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

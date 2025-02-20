module ARexpSpec where

import Test.Hspec
import ARexp
import Rexp
import Val
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "internalise" $ do
        it "converts ZERO to AZERO" $ do
            internalise ZERO `shouldBe` AZERO

        it "converts ONE to AONE with empty bits" $ do
            internalise ONE `shouldBe` AONE []

        it "converts CHAR c to ACHAR c with empty bits" $ do
            internalise (CHAR 'a') `shouldBe` ACHAR 'a' []

        it "converts SEQ to ASEQ with empty bits" $ do
            internalise (SEQ (CHAR 'a') (CHAR 'b')) `shouldBe` ASEQ (ACHAR 'a' []) (ACHAR 'b' []) []

        it "converts SEQ to AALT with list of ALT and empty bits" $ do
            internalise (ALT (CHAR 'a') (CHAR 'b')) `shouldBe` AALT [ACHAR 'a' [Z], ACHAR 'b' [S]] []

    describe "fuse" $ do
        it "prepends to the beginning of a list of Bits" $ do
            fuse [S] (AONE [Z]) `shouldBe` AONE [S,Z]
            fuse [C 'a'] (ACHAR 'b' [Z]) `shouldBe` ACHAR 'b' [C 'a',Z]
            fuse [Z,S] (AALT [ACHAR 'a' [Z], ACHAR 'b' [S]] [S]) `shouldBe` AALT [ACHAR 'a' [Z], ACHAR 'b' [S]] [Z,S,S]
            fuse [Z,S,S] (ASEQ (ACHAR 'a' []) (ACHAR 'b' []) [Z,Z]) `shouldBe` ASEQ (ACHAR 'a' []) (ACHAR 'b' []) [Z,S,S,Z,Z]

    describe "appendBits" $ do
        it "appends to the end of a list of Bits" $ do
            appendBits [S] (AONE [Z]) `shouldBe` AONE [Z, S]
            appendBits [C 'a'] (ACHAR 'b' [Z]) `shouldBe` ACHAR 'b' [Z, C 'a']
            appendBits [Z,S] (AALT [ACHAR 'a' [Z], ACHAR 'b' [S]] [S]) `shouldBe` AALT [ACHAR 'a' [Z], ACHAR 'b' [S]] [S,Z,S]
            appendBits [Z,S,S] (ASEQ (ACHAR 'a' []) (ACHAR 'b' []) [Z,Z]) `shouldBe` ASEQ (ACHAR 'a' []) (ACHAR 'b' []) [Z,Z,Z,S,S]

    describe "erase" $ do
        it "converts AZERO back to ZERO" $ do
            erase AZERO `shouldBe` ZERO

        it "converts AONE to ONE" $ do
            erase (AONE []) `shouldBe` ONE

        it "converts ACHAR back to CHAR" $ do
            erase (ACHAR 'a' []) `shouldBe` CHAR 'a'

        it "converts ASEQ back to SEQ" $ do
            erase (ASEQ (ACHAR 'a' [S]) (ACHAR 'b' [Z]) [C 'c']) `shouldBe` SEQ (CHAR 'a') (CHAR 'b')

    describe "bnullable" $ do
        it "checks if AZERO is not nullable" $ do
            bnullable AZERO `shouldBe` False

        it "checks if AONE is nullable" $ do
            bnullable (AONE []) `shouldBe` True

        it "bnullable should check nullable ARexp" $ do
            bnullable (AONE [S]) `shouldBe` True
            bnullable (ACHAR 'a' []) `shouldBe` False
            bnullable (AALT [AONE [], ACHAR 'b' []] []) `shouldBe` True

    describe "bmkeps" $ do
        it "retrieves bits from AONE" $ do
            bmkeps (AONE [S, Z]) `shouldBe` [S, Z]

    describe "decode" $ do
        it "decodes an empty bit sequence for ONE" $ do
            decode [] ONE `shouldBe` Just Empty

        it "decodes a character" $ do
            decode [] (CHAR 'b') `shouldBe` Just (Chr 'b')

    describe "code" $ do
        it "encodes Empty as an empty bit sequence" $ do
            code Empty `shouldBe` []

        it "encodes a Left value with Z prefix" $ do
            code (Val.Left Empty) `shouldBe` [Z]

        it "encodes a Right value with S prefix" $ do
            code (Val.Right Empty) `shouldBe` [S]

    describe "recursive structures" $ do
        it "handles AALT with multiple branches" $ do
            erase (AALT [ACHAR 'x' [], ACHAR 'y' []] []) `shouldBe` ALT (CHAR 'x') (CHAR 'y')

        it "handles ASEQ composition" $ do
            erase (ASEQ (ACHAR 'a' []) (ACHAR 'b' []) []) `shouldBe` SEQ (CHAR 'a') (CHAR 'b')

        it "handles ASTAR repetition" $ do
            erase (ASTAR (ACHAR 'c' []) []) `shouldBe` STAR (CHAR 'c')

    describe "bders for extended Rexp" $ do
        it "ANTIMES should handle exact repetition" $ do
            bders (ANTIMES (ACHAR 'a' []) 3 []) "aaa" `shouldBe` AALT [ASEQ AZERO (ANTIMES (ACHAR 'a' []) 2 []) [],AALT [ASEQ AZERO (ANTIMES (ACHAR 'a' []) 1 []) [],ASEQ (AONE [Z]) (ANTIMES (ACHAR 'a' []) 0 []) [Z]] [Z]] []
            bders (ANTIMES (ACHAR 'a' []) 3 []) "aa" `shouldBe` AALT [ASEQ AZERO (ANTIMES (ACHAR 'a' []) 2 []) [],ASEQ (AONE [Z]) (ANTIMES (ACHAR 'a' []) 1 []) [Z]] []

        it " APLUS should handle one or more repetitions" $ do
            bders (APLUS (ACHAR 'a' []) []) "a" `shouldBe` ASEQ (AONE [Z]) (ASTAR (ACHAR 'a' []) []) []
            bders (APLUS (ACHAR 'a' []) []) "aaa" `shouldBe` AALT [ASEQ AZERO (ASTAR (ACHAR 'a' []) []) [],AALT [ASEQ AZERO (ASTAR (ACHAR 'a' []) []) [],ASEQ (AONE [Z]) (ASTAR (ACHAR 'a' []) []) [Z]] [Z]] []

        it "AOPTIONAL should handle zero or one occurrence" $ do
            bders (AOPTIONAL (ACHAR 'a' []) []) "" `shouldBe` AOPTIONAL (ACHAR 'a' []) []
            bders (AOPTIONAL (ACHAR 'a' []) []) "a" `shouldBe` AONE []
            bders (AOPTIONAL (ACHAR 'a' []) []) "aa" `shouldBe` AZERO

        it "ARANGE should match characters in set" $ do
            bders (ARANGE (Set.fromList "abc") []) "a" `shouldBe` AONE [C 'a']
            bders (ARANGE (Set.fromList "abc") []) "d" `shouldBe` AZERO

    describe "POSIX tests" $ do
        it "passes for (aba + ab + a)*" $ do
            blexer (STAR ("aba" Rexp.<|> "ab" Rexp.<|> "a")) "ababa" `shouldBe` Just (
                Stars [
                    Val.Right (Val.Left (Sequ (Chr 'a') (Chr 'b'))),
                    Val.Left (Sequ (Chr 'a') (Sequ (Chr 'b') (Chr 'a')))
                    ]
                )

    describe "blexer" $ do
        it "blexer should recognize valid strings" $ do
            blexer (SEQ (CHAR 'a') (CHAR 'b')) "ab" `shouldBe` Just (Sequ (Chr 'a') (Chr 'b'))
            blexer (ALT (CHAR 'a') (CHAR 'b')) "b" `shouldBe` Just (Val.Right (Chr 'b'))
            blexer (STAR (CHAR 'a')) "aaa" `shouldBe` Just (Stars [Chr 'a', Chr 'a', Chr 'a'])
            blexer (PLUS (CHAR 'a')) "a" `shouldBe` Just (Pls [Chr 'a'])

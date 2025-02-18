module ARexpSpec where

import Test.Hspec
import ARexp
import Rexp (Rexp(..))
import Val (Val(..))

spec :: Spec
spec = do
    describe "internalise" $ do
        it "converts ZERO to AZERO" $ do
            internalise ZERO `shouldBe` AZERO

        it "converts ONE to AONE with empty bits" $ do
            internalise ONE `shouldBe` AONE []

        it "converts CHAR c to ACHAR c with empty bits" $ do
            internalise (CHAR 'a') `shouldBe` ACHAR 'a' []

    describe "erase" $ do
        it "converts AZERO back to ZERO" $ do
            erase AZERO `shouldBe` ZERO

        it "converts AONE to ONE" $ do
            erase (AONE []) `shouldBe` ONE

        it "converts ACHAR back to CHAR" $ do
            erase (ACHAR 'a' []) `shouldBe` CHAR 'a'

    describe "bnullable" $ do
        it "checks if AZERO is not nullable" $ do
            bnullable AZERO `shouldBe` False

        it "checks if AONE is nullable" $ do
            bnullable (AONE []) `shouldBe` True

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

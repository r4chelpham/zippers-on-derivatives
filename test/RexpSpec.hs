module RexpSpec where
import Test.QuickCheck
import Test.Hspec
import Rexp

spec :: Spec
spec = do
    describe "nullable" $ do
        it "returns false for ZERO" $ do
            nullable ZERO `shouldBe` False
        
        it "returns true for ONE" $ do
            nullable ONE `shouldBe` True

        it "returns false for CHAR" $ do
            nullable (CHAR 'c') `shouldBe` False

        it "returns true for ALT when either Rexp is nullable" $ do
            nullable (ALT ONE ZERO) `shouldBe` True
            nullable (ALT ZERO ONE) `shouldBe` True

        it "returns false for ALT when neither Rexp is nullable" $ do
            nullable (ALT ZERO ZERO) `shouldBe` False

        it "returns true for SEQ when both Rexp are nullable" $ do
            nullable (SEQ ONE ONE) `shouldBe` True

        it "returns false for SEQ when either Rexp is nullable but not both" $ do
            nullable (SEQ ONE ZERO) `shouldBe` False
            nullable (SEQ ZERO ONE) `shouldBe` False

        it "returns false for SEQ when neither Rexp is nullable" $ do
            nullable (SEQ ZERO ZERO) `shouldBe` False

        it "returns true for all STAR" $ do
            nullable (STAR (CHAR 'c')) `shouldBe` True
            nullable (STAR ONE) `shouldBe` True
            nullable (STAR ZERO) `shouldBe` True

    describe "der" $ do
        it "the derivative of ZERO is ZERO" $ do
            property $ \c -> der ZERO c == ZERO

        it "the derivative of ONE is ZERO" $ do
            property $ \c -> der ONE c == ZERO

        it "the derivative of CHAR c wrt to d is ONE if c and d are the same" $ do
            property $ \c d ->
                if c == d
                then der (CHAR c) d == ONE
                else der (CHAR c) d /= ONE

        it "the derivative of ALT returns an ALT" $ do
            property $ \c d e ->
                case der (ALT c d) e of
                    ALT _ _ -> True
                    _ -> False

        it "the derivative of SEQ returns a SEQ if the first part of the SEQ is not nullable" $ do
            property $ \c d e ->
                not (nullable c) ==> case der (SEQ c d) e of
                        SEQ _ _ -> True
                        _ -> False

        it "the derivative of SEQ returns an ALT if the first part of the SEQ is nullable" $ do
            property $ \c d e ->
                nullable c ==> case der (SEQ c d) e of
                        ALT _ _ -> True
                        _ -> False

        it "the derivative of STAR returns a SEQ with a STAR at the end" $ do
            property $ \c e -> 
                case der (STAR c) e of 
                    SEQ _ (STAR _) -> True
                    _ -> False
        
        it "derivative of a sequence of characters" $ do
            der (SEQ (CHAR 'a') (SEQ (CHAR 'b') (CHAR 'c'))) 'a' `shouldBe` SEQ 1 (SEQ (CHAR 'b') (CHAR 'c'))
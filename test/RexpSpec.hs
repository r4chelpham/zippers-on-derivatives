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

        it "returns false for all RANGE" $ property $ 
            \vs -> not $ nullable (Rexp.RANGE vs)

        it "returns true for all OPTIONAL" $ property $ 
            \r -> nullable (Rexp.OPTIONAL r)

        it "returns true for all PLUS iff r is nullable" $ property $ 
            \r -> nullable (Rexp.PLUS r) ==> nullable r
        
        it "returns true for all NTIMES iff n is 0" $ property $ 
            \r -> nullable (Rexp.NTIMES r 0)

        it "returns true for all NTIMES iff n is not 0 and r is nullable" $ property $ 
            \r n -> n > 0 && nullable (Rexp.NTIMES r n) ==> nullable r 

    describe "der" $ do
        it "derives ZERO from ZERO for any character" $ property $ 
            \c -> der ZERO c == ZERO

        it "derives ZERO from ONE for any character" $ property $ 
            \c -> der ONE c == ZERO

        it "derives ONE from a CHAR d for any character c if c and d are the same" $ property $ 
            \c d ->
                if c == d
                then der (CHAR c) d == ONE
                else der (CHAR c) d /= ONE

        it "derives ALT from an ALT" $ property $ 
            \c d e ->
                case der (ALT c d) e of
                    ALT _ _ -> True
                    _ -> False

        it "derives SEQ from a SEQ if the first part of the SEQ is not nullable" $ property $ 
            \c d e ->
                not (nullable c) ==> case der (SEQ c d) e of
                        SEQ _ _ -> True
                        _ -> False

        it "derives ALT from a SEQ if the first part of the SEQ is nullable" $ property $ 
            \c d e ->
                nullable c ==> case der (SEQ c d) e of
                        ALT _ _ -> True
                        _ -> False

        it "derives SEQ with a STAR at the end from STAR" $ property $ 
            \c e -> 
                case der (STAR c) e of 
                    SEQ _ (STAR _) -> True
                    _ -> False
        
        it "derives a sequence of characters" $ do
            der (SEQ (CHAR 'a') (SEQ (CHAR 'b') (CHAR 'c'))) 'a' 
            `shouldBe` SEQ ONE (SEQ (CHAR 'b') (CHAR 'c'))

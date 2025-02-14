module RexpZipperSpec where

import Test.QuickCheck
import Test.Hspec
import Control.Exception (evaluate)
import qualified RexpZipper as Z

spec :: Spec
spec = do
    describe "focus" $ do
        it "creates a zipper with the given expression" $ do
            let Z.Zipper r _ = Z.focus (Z.CHAR 'a')
            r `shouldBe` Z.CHAR 'a'

    describe "simp" $ do
        it "simplifies SEQ" $ do
            let r = Z.defaultSEQ [Z.ONE, Z.CHAR 'a', Z.CHAR 'b', Z.ONE, Z.CHAR 'c', Z.CHAR 'd', Z.ONE]
            Z.simp r `shouldBe` Z.defaultSEQ [Z.CHAR 'a', Z.CHAR 'b', Z.CHAR 'c', Z.CHAR 'd']

        it "simplifies ALT" $ do
            let r = Z.ALT [Z.ZERO, Z.CHAR 'c', Z.CHAR 'd', Z.ZERO, Z.CHAR 'a', Z.CHAR 'b', Z.ZERO]
            Z.simp r `shouldBe` Z.ALT [Z.CHAR 'c', Z.CHAR 'd', Z.CHAR 'a', Z.CHAR 'b']

        it "simplifies STAR" $ do
            let r = Z.defaultSTAR(Z.defaultSTAR (Z.defaultSTAR (Z.defaultSTAR (Z.CHAR 'a'))))
            Z.simp r `shouldBe` Z.defaultSTAR (Z.CHAR 'a')
        
        it "simplifies the inner expressions of a regular expression as well as the outer expression" $ do
            let r0 = Z.ALT [Z.ZERO, Z.CHAR 'c', Z.CHAR 'd', Z.ZERO, Z.CHAR 'a', Z.CHAR 'b', Z.ZERO]
            let r1 = Z.defaultSTAR(Z.defaultSTAR (Z.defaultSTAR (Z.defaultSTAR (Z.CHAR 'a'))))
            let r2 = Z.defaultSEQ [Z.ONE, Z.CHAR 'a', Z.CHAR 'b', Z.ONE, Z.CHAR 'c', Z.CHAR 'd', Z.ONE]
            let r = Z.ALT [r0, r1, r2]
            Z.simp r `shouldBe` Z.ALT [
                Z.ALT [Z.CHAR 'c', Z.CHAR 'd', Z.CHAR 'a', Z.CHAR 'b'] 
                , Z.defaultSTAR (Z.CHAR 'a')
                , Z.defaultSEQ [Z.CHAR 'a', Z.CHAR 'b', Z.CHAR 'c', Z.CHAR 'd']]

    describe "der" $ do
        it "derivative of ZERO is always empty" $ do
            Z.der 'a' (Z.focus Z.ZERO) `shouldBe` []

        it "derivative of ONE with epsilon character produces empty SEQ" $ do
            let result = Z.der '\0' (Z.focus Z.ONE)
            result `shouldBe` []

        it "derivative of a character with itself produces an empty SEQ" $ do
            let result = Z.der 'a' (Z.focus (Z.CHAR 'a'))
            result `shouldBe` [Z.Zipper (Z.SEQ 'a' []) (Z.SeqC Z.TopC '\NUL' [Z.CHAR 'a'] [])]

        it "derivative of a character with a different character is empty" $ do
            Z.der 'b' (Z.focus (Z.CHAR 'a')) `shouldBe` []

        it "derivative of ZERO is always empty" $ property $ 
            \c -> null (Z.der c (Z.focus Z.ZERO))

        it "derivative of ALT with matching character should return correct result" $ do
            let r = Z.ALT [Z.CHAR 'a', Z.CHAR 'b', Z.CHAR 'c']
            Z.der 'b' (Z.focus r) `shouldBe` [
                Z.Zipper (Z.SEQ 'b' []) 
                (Z.AltC 
                    (Z.SeqC Z.TopC '\0' [Z.ALT [Z.CHAR 'a',Z.CHAR 'b',Z.CHAR 'c']] [])
                )]

        it "derivative of STAR should allow repetition" $ do
            let r = Z.defaultSTAR (Z.CHAR 'a')
            let result = Z.der 'a' (Z.focus r)
            result `shouldNotBe` []

        it "derivative of SEQ with STAR should process correctly" $ do
            let r = Z.SEQ '\0' [Z.defaultSTAR (Z.CHAR 'a'), Z.CHAR 'b']
            let result = Z.der 'a' (Z.focus r)
            result `shouldNotBe` []

        it "derivative of SEQ with ALT should select correct branch" $ do
            let r = Z.SEQ '\0' [Z.ALT [Z.CHAR 'a', Z.CHAR 'b'], Z.CHAR 'c']
            let resultA = Z.der 'a' (Z.focus r)
            let resultB = Z.der 'b' (Z.focus r)
            resultA `shouldNotBe` []
            resultB `shouldNotBe` []

        it "derivative of NTIMES should decrease count when matching" $ do
            let e = Z.defaultNTIMES 3 (Z.CHAR 'a')
            let result = Z.der 'a' (Z.focus e)
            result `shouldNotBe` []

        it "derivative of NTIMES should fail if unmatched character" $ do
            let e = Z.defaultNTIMES 3 (Z.CHAR 'a')
            let result = Z.der 'b' (Z.focus e)
            result `shouldBe` []

    describe "ders" $ do
        it "multiple derivatives of a simple sequence" $ do
            let r = Z.SEQ '\0' [Z.CHAR 'a', Z.CHAR 'b']
            let result = Z.ders "ab" [Z.focus r]
            result `shouldNotBe` []

        it "multiple derivatives of an empty sequence should be empty" $ do
            let r = Z.SEQ '\0' []
            let result = Z.ders "ab" [Z.focus r]
            result `shouldBe` []

        it "should repeat correctly for NTIMES with STAR inside" $ do
            let r = Z.defaultNTIMES 2 (Z.defaultSTAR (Z.CHAR 'a'))
            let result = Z.ders "aaa" [Z.focus r]
            result `shouldNotBe` []

        it "within SEQ should process correctly for NTIMES" $ do
            let r = Z.SEQ 's' [Z.defaultNTIMES 2 (Z.CHAR 'a'), Z.CHAR 'b']
            let result = Z.ders "aab" [Z.focus r]
            result `shouldNotBe` []

        it "should match one or more repetitions for PLUS" $ do
            let r = Z.defaultPLUS (Z.CHAR 'a')
            let result = Z.ders "aaa" [Z.focus r]
            result `shouldNotBe` []

        it "should fail if character does not match for PLUS" $ do
            let r = Z.defaultPLUS (Z.CHAR 'a')
            let result = Z.ders "b" [Z.focus r]
            result `shouldBe` []

    describe "matcher" $ do
        it "matches a simple string" $ do
            let r = Z.SEQ '\0' [Z.CHAR 'a', Z.CHAR 'b']
            Z.matcher "ab" r `shouldBe` True

        it "does not match a different string" $ do
            let r = Z.SEQ '\0' [Z.CHAR 'a', Z.CHAR 'b']
            Z.matcher "ac" r `shouldBe` False

        it "matches an empty string with ONE" $ do
            Z.matcher "" Z.ONE `shouldBe` True

        it "does not match an empty string with a non-empty rression" $ do
            Z.matcher "" (Z.CHAR 'a') `shouldBe` False

        it  "recognises its corresponding sequence" $ property $
            \s -> Z.matcher s (Z.SEQ '\0' (map Z.CHAR s))

        it "only matches empty strings to ONE" $ property $
            \s -> Z.matcher s Z.ONE == (s == "" && s /= "\0") 

        it "recognises repetitions with STAR" $ do
            let r = Z.defaultSTAR (Z.CHAR 'a')
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "a" r `shouldBe` True
            Z.matcher "" r `shouldBe` True
            Z.matcher "b" r `shouldBe` False

        it "correctly evaluates STAR with SEQ" $ do
            let r = Z.SEQ '\0' [Z.defaultSTAR (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "aab" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "a" r `shouldBe` False
            Z.matcher "ba" r `shouldBe` False

        it "correctly evaluates ALT inside STAR" $ do
            let r = Z.defaultSTAR (Z.ALT [Z.CHAR 'a', Z.CHAR 'b'])
            Z.matcher "abba" r `shouldBe` True
            Z.matcher "aaabbb" r `shouldBe` True
            Z.matcher "abc" r `shouldBe` False
            Z.matcher "" r `shouldBe` True

        it "should recognize exact N repetitions" $ do
            let r = Z.defaultNTIMES 3 (Z.CHAR 'a')
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "aa" r `shouldBe` False
            Z.matcher "aaaa" r `shouldBe` False
            Z.matcher "" r `shouldBe` False

        it "should work with NTIMES followed by other characters" $ do
            let r = Z.SEQ 's' [Z.defaultNTIMES 2 (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "aab" r `shouldBe` True
            Z.matcher "ab" r `shouldBe` False
            Z.matcher "aa" r `shouldBe` False
        
        it "should allow sequences of at least one repetition for PLUS inside SEQ" $ do
            let r = Z.SEQ 's' [Z.defaultPLUS (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "ab" r `shouldBe` True
            Z.matcher "aab" r `shouldBe` True
            Z.matcher "b" r `shouldBe` False 

        it "should allow multiple valid choices for PLUS inside ALT" $ do
            let r = Z.ALT [Z.defaultPLUS (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "c" r `shouldBe` False 

        it " should behave like STAR when a PLUS is inside STAR" $ do
            let r = Z.defaultSTAR (Z.defaultPLUS (Z.CHAR 'a'))
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "" r `shouldBe` True

        it "should match either the character or empty string for OPTIONAL" $ do
            let r = Z.defaultOPTIONAL (Z.CHAR 'a')
            Z.matcher "a" r `shouldBe` True
            Z.matcher "" r `shouldBe` True
            Z.matcher "b" r `shouldBe` False 

        it "should allow skipping an element for OPTIONAL inside SEQ" $ do
            let r = Z.SEQ 's' [Z.defaultOPTIONAL (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "ab" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "a" r `shouldBe` False 

        it "should allow an empty choice for an OPTIONAL inside ALT" $ do
            let r = Z.ALT [Z.defaultOPTIONAL (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "a" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "" r `shouldBe` True

        it "should behave like STAR when an OPTIONAL is inside STAR" $ do
            let r = Z.defaultSTAR (Z.defaultOPTIONAL (Z.CHAR 'a'))
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "" r `shouldBe` True 
            Z.matcher "b" r `shouldBe` False 

        {- 
            an example that does not work: (a*)*b
            a* is evaluated and its result is never remembered so when we try to go up from
            the failed result, we try again and again and again... so it infinitely loops
             - so we have to find a way to remember this - memoisation
         -}

    describe "flatten" $ do
        it "throws an error when flattening ZERO" $ do
            evaluate (Z.flatten Z.ZERO) `shouldThrow` errorCall "Cannot flatten ZERO"

        it "returns an empty list when flattening ONE" $ do
            Z.flatten Z.ONE `shouldBe` []

        it "returns an empty list when flattening CHAR" $ do
            Z.flatten (Z.CHAR 'a') `shouldBe` []

        it "returns a list with the char when flattening SEQ with empty list" $ do
            Z.flatten (Z.SEQ 'a' []) `shouldBe` ['a']

        it "returns a concatenated list of flattened expressions for SEQ" $ do
            Z.flatten (Z.SEQ 'a' [Z.CHAR 'b', Z.SEQ 'c' [Z.ONE]]) `shouldBe` ['a', 'c']

        it "returns a concatenated list of flattened expressions for ALT" $ do
            Z.flatten (Z.ALT [Z.CHAR 'a', Z.SEQ 'b' [Z.ONE], Z.SEQ 'c' []]) `shouldBe` ['b', 'c']

        it "returns a concatenated list of flattened expressions for STAR" $ do
            Z.flatten (Z.STAR (Z.ALT [Z.CHAR 'b', Z.CHAR 'c']) [Z.SEQ 'b' [Z.ONE], Z.SEQ 'c' []]) `shouldBe` ['b', 'c']

        it "returns a flattened list from the nested expression in RECD" $ property $
            \r -> Z.flatten (Z.RECD "1" r [Z.SEQ 'a' [Z.SEQ 'b' [], Z.ONE]]) `shouldBe` ['a', 'b']

        it "correctly flattens deeply nested expressions" $ do
            Z.flatten (Z.ALT [Z.SEQ 'x' [Z.STAR (Z.CHAR 'z') []], Z.SEQ 'a' [Z.ONE]]) `shouldBe` ['x', 'a']

        it "NTIMES inside ALT should allow correct branching" $ do
            let r = Z.ALT [Z.defaultNTIMES 2 (Z.CHAR 'a'), Z.CHAR 'b']
            let resultA = Z.ders "aa" [Z.focus r]
            let resultB = Z.ders "b" [Z.focus r]
            resultA `shouldNotBe` []
            resultB `shouldNotBe` []
        

        
        
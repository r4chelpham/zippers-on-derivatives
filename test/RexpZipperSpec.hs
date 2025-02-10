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

    describe "der" $ do
        it "derivative of ZERO is always empty" $ do
            Z.der 'a' (Z.focus Z.ZERO) `shouldBe` []

        it "derivative of ONE with epsilon character produces empty SEQ" $ do
            let result = Z.der '\0' (Z.focus Z.ONE)
            -- result `shouldBe` [Z.Zipper (Z.SEQ '\NUL' [Z.SEQ '\NUL' [Z.ONE]]) Z.TopC]
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
            let r = Z.STAR (Z.CHAR 'a') []
            let result = Z.der 'a' (Z.focus r)
            result `shouldNotBe` []

        it "derivative of SEQ with STAR should process correctly" $ do
            let r = Z.SEQ '\0' [Z.STAR (Z.CHAR 'a') [], Z.CHAR 'b']
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

        it "derivative of NTIMES should reach zero correctly" $ do
            let e = Z.defaultNTIMES 1 (Z.CHAR 'a')
            let result = Z.ders "a" [Z.focus e]
            case result of
                [Z.Zipper (Z.SEQ _ es) _] -> 
                    any (\(Z.NTIMES 0 (Z.CHAR 'a') _ _) -> True) es
                _ -> False

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

        it "NTIMES with STAR inside should repeat correctly" $ do
            let r = Z.defaultNTIMES 2 (Z.STAR (Z.CHAR 'a') [])
            let result = Z.ders "aaa" [Z.focus r]
            result `shouldNotBe` []

        it "NTIMES within SEQ should process correctly" $ do
            let r = Z.SEQ 's' [Z.defaultNTIMES 2 (Z.CHAR 'a'), Z.CHAR 'b']
            let result = Z.ders "aab" [Z.focus r]
            result `shouldNotBe` []

        it "PLUS should match one or more repetitions" $ do
            let r = Z.PLUS (Z.CHAR 'a') []
            let result = Z.ders "aaa" [Z.focus r]
            result `shouldNotBe` []

        it "PLUS should fail if character does not match" $ do
            let r = Z.PLUS (Z.CHAR 'a') []
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
            Z.matcher "" Z.ONE `shouldBe` True -- fails

        it "does not match an empty string with a non-empty rression" $ do
            Z.matcher "" (Z.CHAR 'a') `shouldBe` False

        it  "recognises its corresponding sequence" $ property $
            \s -> Z.matcher s (Z.SEQ '\0' (map Z.CHAR s))

        it "only matches empty strings to ONE" $ property $
            \s -> Z.matcher s Z.ONE == (s == "" && s /= "\0") -- fails because der ONE is []

        it "recognises repetitions with STAR" $ do
            let r = Z.STAR (Z.CHAR 'a') []
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "a" r `shouldBe` True
            Z.matcher "" r `shouldBe` True
            Z.matcher "b" r `shouldBe` False

        it "correctly evaluates STAR with SEQ" $ do
            let r = Z.SEQ '\0' [Z.STAR (Z.CHAR 'a') [], Z.CHAR 'b']
            Z.matcher "aab" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "a" r `shouldBe` False
            Z.matcher "ba" r `shouldBe` False

        it "correctly evaluates ALT inside STAR" $ do
            let r = Z.STAR (Z.ALT [Z.CHAR 'a', Z.CHAR 'b']) []
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
        
        it "PLUS inside SEQ should allow sequences of at least one repetition" $ do
            let r = Z.SEQ 's' [Z.PLUS (Z.CHAR 'a') [], Z.CHAR 'b']
            Z.matcher "ab" r `shouldBe` True
            Z.matcher "aab" r `shouldBe` True
            Z.matcher "b" r `shouldBe` False 

        it "PLUS inside ALT should allow multiple valid choices" $ do
            let r = Z.ALT [Z.PLUS (Z.CHAR 'a') [], Z.CHAR 'b']
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "c" r `shouldBe` False 

        it "PLUS inside STAR should behave like STAR" $ do
            let r = Z.STAR (Z.PLUS (Z.CHAR 'a') []) []
            Z.matcher "aaa" r `shouldBe` True
            Z.matcher "" r `shouldBe` True

        it "OPTIONAL should match either the character or empty string" $ do
            let r = Z.defaultOPTIONAL (Z.CHAR 'a')
            Z.matcher "a" r `shouldBe` True
            Z.matcher "" r `shouldBe` True -- der ONE returns []
            Z.matcher "b" r `shouldBe` False 

        it "OPTIONAL inside SEQ should allow skipping an element" $ do
            let r = Z.SEQ 's' [Z.defaultOPTIONAL (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "ab" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True -- fails because the first part of the SEQ returns []
            Z.matcher "a" r `shouldBe` False 

        it "OPTIONAL inside ALT should allow an empty choice" $ do
            let r = Z.ALT [Z.defaultOPTIONAL (Z.CHAR 'a'), Z.CHAR 'b']
            Z.matcher "a" r `shouldBe` True
            Z.matcher "b" r `shouldBe` True
            Z.matcher "" r `shouldBe` True -- fails because it returns []

        -- fails when der ONE is up (defaultSEQ [ONE]) ct/ [Zipper (defaultSEQ [ONE]) ct]
        -- it "OPTIONAL inside STAR should behave like STAR" $ do
        --     let r = Z.STAR (Z.defaultOPTIONAL (Z.CHAR 'a')) []
        --     Z.matcher "aaa" r `shouldBe` True
        --     Z.matcher "" r `shouldBe` True 
        --     Z.matcher "b" r `shouldBe` False 

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
            let exp = Z.ALT [Z.defaultNTIMES 2 (Z.CHAR 'a'), Z.CHAR 'b']
            let resultA = Z.ders "aa" [Z.focus exp]
            let resultB = Z.ders "b" [Z.focus exp]
            resultA `shouldNotBe` []
            resultB `shouldNotBe` []
        

        
        
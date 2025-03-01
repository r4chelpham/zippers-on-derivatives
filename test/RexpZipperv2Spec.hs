module RexpZipperv2Spec where

import Test.QuickCheck
import Test.Hspec
import qualified RexpZipperv2 as Z

{- | Due to the mutable nature of the final matching algorithm,
we need to create instances of each Exp every time to test.

As a result, tests may be split up into parts , denoted with (n)
where n is the nth part of the test.
-}
spec :: Spec
spec = do
    describe "Matcher test cases:" $ do
        it "matches a CHAR to a single character" $ do
            a <- Z.createExp (Z.CHAR 'a')
            es <- Z.run "a" a
            Z.matcher es `shouldBe` True
        
        it "does not match a CHAR to any other characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            es <- Z.run "b" a
            Z.matcher es `shouldBe` False
        
        it "does not match a CHAR to the empty string" $ do
            a <- Z.createExp (Z.CHAR 'a')
            es <- Z.run "" a
            Z.matcher es `shouldBe` False

        it "matches a SEQ to a sequence of characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            c <- Z.createExp (Z.CHAR 'c')
            d <- Z.createExp (Z.CHAR 'd')
            e <- Z.createExp (Z.SEQ '\0' [a,b,c,d])
            es <- Z.run "abcd" e
            Z.matcher es `shouldBe` True
            
        it "does not match a SEQ to an incomplete sequence of characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            c <- Z.createExp (Z.CHAR 'c')
            d <- Z.createExp (Z.CHAR 'd')
            e <- Z.createExp (Z.SEQ '\0' [a,b,c,d])
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` False

        it "does not match a SEQ to a sequence of characters containing the matched sequence" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            c <- Z.createExp (Z.CHAR 'c')
            d <- Z.createExp (Z.CHAR 'd')
            e <- Z.createExp (Z.SEQ '\0' [a,b,c,d])
            es <- Z.run "abcdefg" e
            Z.matcher es `shouldBe` False

        it "matches a nullable SEQ to the empty string" $ do
            e1 <- Z.createExp (Z.SEQ '\0' [])
            e2 <- Z.createExp (Z.SEQ '\0' [])
            e <- Z.createExp (Z.SEQ '\0' [e1, e2])
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches an ALT to either of its characters (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True
        
        it "matches an ALT to either of its characters (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            es <- Z.run "b" e
            Z.matcher es `shouldBe` True
        
        it "does not match an ALT to a sequence of both characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` False

        it "does not match a non nullable ALT to the empty string" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            es <- Z.run "" e
            Z.matcher es `shouldBe` False

        it "matches a nullable ALT to the empty string" $ do
            e1 <- Z.createExp (Z.SEQ '\0' [])
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [e1,b])
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        -- | TODO: try to write a property for this instead
        it "matches a STAR to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches a STAR to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches a STAR to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "aaaaaaa" e
            Z.matcher es `shouldBe` True

        it "matches a STAR to 0 or more repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "aaaaaaaaaaaaaaaa" e
            Z.matcher es `shouldBe` True

        -- | TODO: try to write a property for this instead
        it "matches a STAR of SEQ to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches a STAR of SEQ to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` True

        it "matches a STAR of SEQ to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "abab" e
            Z.matcher es `shouldBe` True

        it "matches a STAR of SEQ to 0 or more repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "ababababab" e
            Z.matcher es `shouldBe` True

        it "does not match a STAR of SEQ to any repetition of a string that does not match the sequence" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "abbabab" e
            Z.matcher es `shouldBe` False

        -- | TODO: try to write a property for this instead
        it "matches a STAR of ALT to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches a STAR of ALT to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches a STAR of ALT to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "b" e
            Z.matcher es `shouldBe` True

        it "matches a STAR of ALT to 0 or more repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "abbaabab" e
            Z.matcher es `shouldBe` True

        it "does not match a STAR of ALT to any repetition of a string that does not match the alternative" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            es <- Z.run "c" e
            Z.matcher es `shouldBe` False
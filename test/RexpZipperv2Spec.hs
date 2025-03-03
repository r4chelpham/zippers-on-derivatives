{-# LANGUAGE ScopedTypeVariables #-}
module RexpZipperv2Spec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified RexpZipperv2 as Z
import GHC.IORef

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

        it "matches a SEQ containing a STAR (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "b" e
            Z.matcher es `shouldBe` True

        it "matches a SEQ containing a STAR (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` True
        
        it "matches a SEQ containing a STAR (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "" e
            Z.matcher es `shouldBe` False

        it "matches a SEQ containing a STAR (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "aaaaab" e
            Z.matcher es `shouldBe` True

        it "matches a SEQ containing a PLUS (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "b" e
            Z.matcher es `shouldBe` False

        it "matches a SEQ containing a PLUS (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` True
        
        it "matches a SEQ containing a PLUS (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "" e
            Z.matcher es `shouldBe` False

        it "matches a SEQ containing a PLUS (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "aaaab" e
            Z.matcher es `shouldBe` True

        it "matches a SEQ containing a PLUS (5)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "aaaaa" e
            Z.matcher es `shouldBe` False

        it "matches a SEQ containing a OPTIONAL (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "b" e
            Z.matcher es `shouldBe` True

        it "matches a SEQ containing a OPTIONAL (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` True
        
        it "matches a SEQ containing a OPTIONAL (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            es <- Z.run "" e
            Z.matcher es `shouldBe` False

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

        -- | TODO: try to write a property for this instead
        it "matches a PLUS to 1 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches a PLUS to 1 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            es <- Z.run "aaaa" e
            Z.matcher es `shouldBe` True

        it "matches a PLUS to 1 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            es <- Z.run "aaaaaaa" e
            Z.matcher es `shouldBe` True

        it "matches a PLUS to 1 or more repetitions of itself (does not match the empty string) (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            es <- Z.run "" e
            Z.matcher es `shouldBe` False

        -- | Equivalences: (r*)+ == r*
        it "matches an PLUS of a STAR to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.PLUS e')
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches an PLUS of a STAR to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.PLUS e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches an PLUS of a STAR to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.PLUS e')
            es <- Z.run "aaaaaa" e
            Z.matcher es `shouldBe` True

        -- | Equivalences: (r?)+ == r*
        it "matches an PLUS of a OPTIONAL to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.PLUS e')
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches an PLUS of a OPTIONAL to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.PLUS e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches an PLUS of a OPTIONAL to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.PLUS e')
            es <- Z.run "aaaaaa" e
            Z.matcher es `shouldBe` True

        -- | TODO: try to write a property for this instead
        it "matches an OPTIONAL to the empty string" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches an OPTIONAL to itself" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True
        
        it "does not match an OPTIONAL to more than 1 repetition of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            es <- Z.run "aa" e
            Z.matcher es `shouldBe` False

        it "does not match an OPTIONAL to more than 1 repetition of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            es <- Z.run "aaa" e
            Z.matcher es `shouldBe` False

        it "does not match an OPTIONAL to more than 1 repetition of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            es <- Z.run "aaaaaaaaaaaa" e
            Z.matcher es `shouldBe` False

        -- | Equivalences: (r*)? == r*
        it "matches an OPTIONAL of a STAR to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.OPTIONAL e')
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches an OPTIONAL of a STAR to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.OPTIONAL e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches an OPTIONAL of a STAR to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.OPTIONAL e')
            es <- Z.run "aaaaa" e
            Z.matcher es `shouldBe` True

        -- | Equivalences: (r+)? == r*
        it "matches an OPTIONAL of a PLUS to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.OPTIONAL e')
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches an OPTIONAL of a PLUS to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.OPTIONAL e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches an OPTIONAL of a PLUS to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.OPTIONAL e')
            es <- Z.run "aaaaa" e
            Z.matcher es `shouldBe` True

        it "matches an NTIMES to exactly n repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            es <- Z.run "aaa" e
            Z.matcher es `shouldBe` True

        it "matches an NTIMES to exactly n repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            es <- Z.run "" e
            Z.matcher es `shouldBe` False

        it "matches an NTIMES to exactly n repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            es <- Z.run "aaaa" e
            Z.matcher es `shouldBe` False

        it "matches an NTIMES to exactly n repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            es <- Z.run "a" e
            Z.matcher es `shouldBe` False

        -- | Equivalences: (r?)^n == (r)^{..n}
        it "matches nullable NTIMES (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches nullable NTIMES (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            es <- Z.run "aa" e
            Z.matcher es `shouldBe` True

        it "matches nullable NTIMES (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            es <- Z.run "aaa" e
            Z.matcher es `shouldBe` True

        it "matches nullable NTIMES (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            es <- Z.run "aaaa" e
            Z.matcher es `shouldBe` False

        -- | TODO: create property based tests for this instead
        it "matches a RECD just like it would match a regular CHAR (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.RECD "s" a)
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular CHAR (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.RECD "s" a)
            es <- Z.run "b" e
            Z.matcher es `shouldBe` False

        it "matches a RECD just like it would match a regular SEQ (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular SEQ (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` False

        it "matches a RECD just like it would match a regular SEQ (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "b" e
            Z.matcher es `shouldBe` False

        it "matches a RECD just like it would match a regular ALT (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "a" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular ALT (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "b" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular ALT (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "ab" e
            Z.matcher es `shouldBe` False

        it "matches a RECD just like it would match a regular STAR (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular STAR (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "aaaaa" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular STAR (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "b" e
            Z.matcher es `shouldBe` False

        it "matches a RECD just like it would match a regular PLUS (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular PLUS (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "aaaaa" e
            Z.matcher es `shouldBe` True

        it "matches a RECD just like it would match a regular PLUS (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "b" e
            Z.matcher es `shouldBe` False

        it "flattens sequences accordingly" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "aaaaa" e
            e' <- readIORef (Z.exp' (head es))
            res <- Z.flatten e'
            res  `shouldBe` "aaaaa"

        it "flattens alternates accordingly" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR e1)
            es <- Z.run "abba" e
            e' <- readIORef (Z.exp' (head es))
            res <- Z.flatten e'
            res `shouldBe` "abba"

        it "flattens records accordingly" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "aaaaa" e
            e' <- readIORef (Z.exp' (head es))
            res <- Z.flatten e'
            res `shouldBe` "aaaaa"

        it "only tokenises on records (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "aaaaa" e
            e' <- readIORef (Z.exp' (head es))
            res <- Z.env e'
            res `shouldBe` [("s", "aaaaa")]
        
        it "only tokenises on records (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "aaaaa" e
            e' <- readIORef (Z.exp' (head es))
            res <- Z.env e'
            res  `shouldBe` []

        it "only tokenises on records (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR e1)
            es <- Z.run "abba" e
            e' <- readIORef (Z.exp' (head es))
            res <- Z.env e'
            res  `shouldBe` []

    describe "String extension tests (toExp')" $ do
        it "converts the empty string to an Exp'" $ do
            e' <- Z.toExp' ""
            e' `shouldBe` Z.SEQ '\0' []
        
        prop "converts a character to an Exp'" $ do
            \c -> ioProperty  $ do
                e' <- Z.toExp' [c] 
                return (e' == Z.CHAR c)

        prop "converts Strings -> Exp'" $ do
            \c cs -> ioProperty $ do
                let s = c:cs
                e' <- Z.toExp' s
                case s of
                    [c] -> return (e' == Z.CHAR c)
                    _ -> do
                        expectedVals <- mapM (Z.createExp . Z.CHAR) s
                        return (e' == Z.SEQ '\0' expectedVals)

    describe "String extension tests (toExp)" $ do
        it "converts the empty string to an Exp" $ do
            (Z.Exp mRef eRef') <- Z.toExp ""
            m <- readIORef mRef
            e' <- readIORef eRef'
            mBott <- Z.mBottom
            m `shouldBe` mBott
            e' `shouldBe` Z.SEQ '\0' []
        
        prop "converts a character to an Exp" $ do
            \c -> ioProperty  $ do
                (Z.Exp mRef eRef') <- Z.toExp [c] 
                m <- readIORef mRef
                e' <- readIORef eRef'
                mBott <- Z.mBottom
                return ((e' == Z.CHAR c) && (m == mBott))

        prop "converts Strings -> Exp" $ do
            \c cs -> ioProperty $ do
                let s = c:cs
                (Z.Exp mRef eRef') <- Z.toExp s
                m <- readIORef mRef
                e' <- readIORef eRef'
                mBott <- Z.mBottom
                case s of
                    [c] -> return ((e' == Z.CHAR c) && (m == mBott))
                    _ -> do
                        expectedVals <- mapM (Z.createExp . Z.CHAR) s
                        return (
                            (e' == Z.SEQ '\0' expectedVals) && 
                            (m == mBott)
                            )

    describe "Extension tests for <|>" $ do
        prop "converts alternate between two strings" $ do
            \(s1 :: [Char]) (s2 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<|> s2
                e1 <- Z.toExp s1
                e2 <- Z.toExp s2
                let e' = Z.ALT [e1, e2]
                expected <- Z.toExp e'
                return (e == expected)

        prop "converts alternate between multiple strings" $ do
            \(s1 :: [Char]) (s2 :: [Char]) (s3 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<|> s2 Z.<|> s3
                e1 <- Z.toExp s1
                e2 <- Z.toExp s2
                e3 <- Z.toExp s3
                let e' = Z.ALT [e1,e2,e3]
                expected <- Z.toExp e'
                return (e == expected)

    describe "Extension tests for <~>" $ do
        prop "converts sequence between two strings" $ do
            \(s1 :: [Char]) (s2 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<~> s2
                e1 <- Z.toExp s1
                e2 <- Z.toExp s2
                let e' = Z.SEQ '\0' [e1, e2]
                expected <- Z.toExp e'
                return (e == expected)

        prop "converts sequence between multiple strings" $ do
            \(s1 :: [Char]) (s2 :: [Char]) (s3 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<~> s2 Z.<~> s3
                e1 <- Z.toExp s1
                e2 <- Z.toExp s2
                e3 <- Z.toExp s3
                let e' = Z.SEQ '\0' [e1,e2,e3]
                expected <- Z.toExp e'
                return (e == expected)
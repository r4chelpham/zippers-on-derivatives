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
            res <- Z.matcher "a" a
            res `shouldBe` True
        
        it "does not match a CHAR to any other characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            res <- Z.matcher "b" a
            res `shouldBe` False
        
        it "does not match a CHAR to the empty string" $ do
            a <- Z.createExp (Z.CHAR 'a')
            res <- Z.matcher  "" a
            res `shouldBe` False

        it "matches a SEQ to a sequence of characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            c <- Z.createExp (Z.CHAR 'c')
            d <- Z.createExp (Z.CHAR 'd')
            e <- Z.createExp (Z.SEQ '\0' [a,b,c,d])
            res <- Z.matcher "abcd" e
            res `shouldBe` True
            
        it "does not match a SEQ to an incomplete sequence of characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            c <- Z.createExp (Z.CHAR 'c')
            d <- Z.createExp (Z.CHAR 'd')
            e <- Z.createExp (Z.SEQ '\0' [a,b,c,d])
            res <- Z.matcher "ab" e
            res `shouldBe` False

        it "does not match a SEQ to a sequence of characters containing the matched sequence" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            c <- Z.createExp (Z.CHAR 'c')
            d <- Z.createExp (Z.CHAR 'd')
            e <- Z.createExp (Z.SEQ '\0' [a,b,c,d])
            res <- Z.matcher "abcdefg" e
            res `shouldBe` False

        it "matches a nullable SEQ to the empty string" $ do
            e1 <- Z.createExp (Z.SEQ '\0' [])
            e2 <- Z.createExp (Z.SEQ '\0' [])
            e <- Z.createExp (Z.SEQ '\0' [e1, e2])
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches a SEQ containing a STAR at the beginning (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "b" e
            res `shouldBe` True

        it "matches a SEQ containing a STAR at the beginning (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "ab" e
            res `shouldBe` True
        
        it "matches a SEQ containing a STAR at the beginning (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches a SEQ containing a STAR at the beginning (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "aaaaab" e
            res `shouldBe` True


        it "matches a SEQ containing a STAR at the end (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [b,e1])
            res <- Z.matcher "b" e
            res `shouldBe` True

        it "matches a SEQ containing a STAR at the end (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [b,e1])
            res <- Z.matcher "ba" e
            res `shouldBe` True
        
        it "matches a SEQ containing a STAR at the end (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [b,e1])
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches a SEQ containing a STAR at the end (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.SEQ '\0' [b,e1])
            res <- Z.matcher "baaaaaaa" e
            res `shouldBe` True

        it "matches a SEQ containing a PLUS (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "b" e
            res `shouldBe` False

        it "matches a SEQ containing a PLUS (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "ab" e
            res `shouldBe` True
        
        it "matches a SEQ containing a PLUS (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches a SEQ containing a PLUS (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "aaaab" e
            res `shouldBe` True

        it "matches a SEQ containing a PLUS (5)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "aaaaa" e
            res `shouldBe` False

        it "matches a SEQ containing a OPTIONAL (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "b" e
            res `shouldBe` True

        it "matches a SEQ containing a OPTIONAL (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "ab" e
            res `shouldBe` True
        
        it "matches a SEQ containing a OPTIONAL (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.SEQ '\0' [e1,b])
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches an ALT to either of its characters (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            res <- Z.matcher "a" e
            res `shouldBe` True
        
        it "matches an ALT to either of its characters (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            res <- Z.matcher "b" e
            res `shouldBe` True
        
        it "does not match an ALT to a sequence of both characters" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            res <- Z.matcher "ab" e
            res `shouldBe` False

        it "does not match a non nullable ALT to the empty string" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [a,b])
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches a nullable ALT to the empty string" $ do
            e1 <- Z.createExp (Z.SEQ '\0' [])
            b <- Z.createExp (Z.CHAR 'b')
            e <- Z.createExp (Z.ALT [e1,b])
            res <- Z.matcher "" e
            res `shouldBe` True

        -- | TODO: try to write a property for this instead
        it "matches a STAR to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches a STAR to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches a STAR to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            res <- Z.matcher "aaaaaaa" e
            res `shouldBe` True

        it "matches a STAR to 0 or more repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            res <- Z.matcher "aaaaaaaaaaaaaaaa" e
            res `shouldBe` True

        -- | TODO: try to write a property for this instead
        it "matches a STAR of SEQ to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches a STAR of SEQ to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "ab" e
            res `shouldBe` True

        it "matches a STAR of SEQ to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "abab" e
            res `shouldBe` True

        it "matches a STAR of SEQ to 0 or more repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "ababababab" e
            res `shouldBe` True

        it "does not match a STAR of SEQ to any repetition of a string that does not match the sequence" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "abbabab" e
            res `shouldBe` False

        -- | TODO: try to write a property for this instead
        it "matches a STAR of ALT to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches a STAR of ALT to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches a STAR of ALT to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "b" e
            res `shouldBe` True

        it "matches a STAR of ALT to 0 or more repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "abbaabab" e
            res `shouldBe` True

        it "does not match a STAR of ALT to any repetition of a string that does not match the alternative" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            se <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.STAR se)
            res <- Z.matcher "c" e
            res `shouldBe` False

        -- | TODO: try to write a property for this instead
        it "matches a PLUS to 1 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches a PLUS to 1 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            res <- Z.matcher "aaaa" e
            res `shouldBe` True

        it "matches a PLUS to 1 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            res <- Z.matcher "aaaaaaa" e
            res `shouldBe` True

        it "matches a PLUS to 1 or more repetitions of itself (does not match the empty string) (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.PLUS a)
            res <- Z.matcher "" e
            res `shouldBe` False

        -- | Equivalences: (r*)+ == r*
        it "matches an PLUS of a STAR to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.PLUS e')
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches an PLUS of a STAR to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.PLUS e')
            res <- Z.matcher "b" e
            res `shouldBe` False

        it "matches an PLUS of a STAR to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.PLUS e')
            res <- Z.matcher "aaaaaa" e
            res `shouldBe` True

        -- | Equivalences: (r?)+ == r*
        it "matches an PLUS of a OPTIONAL to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.PLUS e')
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches an PLUS of a OPTIONAL to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.PLUS e')
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches an PLUS of a OPTIONAL to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.PLUS e')
            res <- Z.matcher "aaaaaa" e
            res `shouldBe` True

        -- | TODO: try to write a property for this instead
        it "matches an OPTIONAL to the empty string" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches an OPTIONAL to itself" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            res <- Z.matcher "a" e
            res `shouldBe` True
        
        it "does not match an OPTIONAL to more than 1 repetition of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            res <- Z.matcher "aa" e
            res `shouldBe` False

        it "does not match an OPTIONAL to more than 1 repetition of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            res <- Z.matcher "aaa" e
            res `shouldBe` False

        it "does not match an OPTIONAL to more than 1 repetition of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.OPTIONAL a)
            res <- Z.matcher "aaaaaaaaaaaa" e
            res `shouldBe` False

        -- | Equivalences: (r*)? == r*
        it "matches an OPTIONAL of a STAR to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.OPTIONAL e')
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches an OPTIONAL of a STAR to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.OPTIONAL e')
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches an OPTIONAL of a STAR to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.OPTIONAL e')
            res <- Z.matcher "aaaaa" e
            res `shouldBe` True

        -- | Equivalences: (r+)? == r*
        it "matches an OPTIONAL of a PLUS to 0 or more repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.OPTIONAL e')
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches an OPTIONAL of a PLUS to 0 or more repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.OPTIONAL e')
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches an OPTIONAL of a PLUS to 0 or more repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.PLUS a)
            e <- Z.createExp (Z.OPTIONAL e')
            res <- Z.matcher "aaaaa" e
            res `shouldBe` True

        it "matches an NTIMES to exactly n repetitions of itself (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            res <- Z.matcher "aaa" e
            res `shouldBe` True

        it "matches an NTIMES to exactly n repetitions of itself (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches an NTIMES to exactly n repetitions of itself (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            res <- Z.matcher "aaaa" e
            res `shouldBe` False

        it "matches an NTIMES to exactly n repetitions of itself (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.NTIMES 3 a)
            res <- Z.matcher "a" e
            res `shouldBe` False

        -- | Equivalences: (r?)^n == (r)^{..n}
        it "matches nullable NTIMES (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches nullable NTIMES (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            res <- Z.matcher "aa" e
            res `shouldBe` True

        it "matches nullable NTIMES (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            res <- Z.matcher "aaa" e
            res `shouldBe` True

        it "matches nullable NTIMES (4)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.OPTIONAL a)
            e <- Z.createExp (Z.NTIMES 3 e')
            res <- Z.matcher "aaaa" e
            res `shouldBe` False

        it "matches nullable NTIMES in a SEQ (1)" $ do
            e <- (("a" Z.?> ()) Z.^> 2) Z.<~> ("a" Z.^> 2)
            res <- Z.matcher "" e
            res `shouldBe` False

        it "matches nullable NTIMES in a SEQ (2)" $ do
            e <- (("a" Z.?> ()) Z.^> 2) Z.<~> ("a" Z.^> 2)
            res <- Z.matcher "a" e
            res `shouldBe` False
        
        it "matches nullable NTIMES in a SEQ (3)" $ do
            e <- (("a" Z.?> ()) Z.^> 2) Z.<~> ("a" Z.^> 2)
            res <- Z.matcher "aa" e
            res `shouldBe` True

        it "matches nullable NTIMES in a SEQ (4)" $ do
            e <- (("a" Z.?> ()) Z.^> 2) Z.<~> ("a" Z.^> 2)
            res <- Z.matcher "aaa" e
            res `shouldBe` True

        it "matches nullable NTIMES in a SEQ (5)" $ do
            e <- (("a" Z.?> ()) Z.^> 2) Z.<~> ("a" Z.^> 2)
            res <- Z.matcher "aaaa" e
            res `shouldBe` True


        -- | TODO: create property based tests for this instead
        it "matches a RECD just like it would match a regular CHAR (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.RECD "s" a)
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular CHAR (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.RECD "s" a)
            res <- Z.matcher "b" e
            res `shouldBe` False

        it "matches a RECD just like it would match a regular SEQ (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "ab" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular SEQ (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "a" e
            res `shouldBe` False

        it "matches a RECD just like it would match a regular SEQ (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.SEQ '\0' [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "b" e
            res `shouldBe` False

        it "matches a RECD just like it would match a regular ALT (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "a" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular ALT (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "b" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular ALT (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e' <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "ab" e
            res `shouldBe` False

        it "matches a RECD just like it would match a regular STAR (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular STAR (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "aaaaa" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular STAR (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "b" e
            res `shouldBe` False

        it "matches a RECD just like it would match a regular PLUS (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular PLUS (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "aaaaa" e
            res `shouldBe` True

        it "matches a RECD just like it would match a regular PLUS (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            res <- Z.matcher "b" e
            res `shouldBe` False

        it "flattens sequences accordingly" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "aaaaa" e
            res <- Z.flatten (head es)
            res  `shouldBe` "aaaaa"

        it "flattens alternates accordingly" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR e1)
            es <- Z.run "abba" e
            res <- Z.flatten (head es)
            res `shouldBe` "abba"

        it "flattens records accordingly" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "aaaaa" e
            res <- Z.flatten (head es)
            res `shouldBe` "aaaaa"

        it "only tokenises on records (1)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e' <- Z.createExp (Z.STAR a)
            e <- Z.createExp (Z.RECD "s" e')
            es <- Z.run "aaaaa" e
            res <- Z.env (head es)
            res `shouldBe` [("s", "aaaaa")]
        
        it "only tokenises on records (2)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            e <- Z.createExp (Z.STAR a)
            es <- Z.run "aaaaa" e
            res <- Z.env (head es)
            res  `shouldBe` []

        it "only tokenises on records (3)" $ do
            a <- Z.createExp (Z.CHAR 'a')
            b <- Z.createExp (Z.CHAR 'b')
            e1 <- Z.createExp (Z.ALT [a,b])
            e <- Z.createExp (Z.STAR e1)
            es <- Z.run "abba" e
            res <- Z.env (head es)
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

        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do

    describe "Extension tests for <|>" $ do
        prop "converts alternate between two strings" $ do
            \(s1 :: [Char]) (s2 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<|> s2
                e1 <- Z.toExp s1
                e2 <- Z.toExp s2
                let e' = Z.ALT [e1, e2]
                expected <- Z.toExp e'
                return (e == expected)

        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do

    describe "Extension tests for <~>" $ do
        prop "converts sequence between two strings" $ do
            \(s1 :: [Char]) (s2 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<~> s2
                e1 <- Z.toExp s1
                e2 <- Z.toExp s2
                let e' = Z.SEQ '\0' [e1, e2]
                expected <- Z.toExp e'
                return (e == expected)

        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do

    describe "Extension tests for <$>" $ do
        prop "converts and labels a string" $ do
            \s1 (s2 :: [Char]) -> ioProperty $ do
                e <- s1 Z.<$> s2
                e' <- Z.toExp s2
                expected <- Z.createExp (Z.RECD s1 e')
                return (e == expected)

        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "labels an Exp'" $ do
        -- prop "labels an Exp" $ do

    describe "Extension tests for *>" $ do
        prop "converts a string" $ do
            \(s :: [Char]) -> ioProperty $ do
                e <- s Z.*> ()
                e' <- Z.toExp s
                expected <- Z.createExp (Z.STAR e')
                return (e == expected)
        
        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do

    describe "Extension tests for +>" $ do   
        prop "converts a string" $ do
            \(s :: [Char]) -> ioProperty $ do
                e <- s Z.+> ()
                e' <- Z.toExp s
                expected <- Z.createExp (Z.PLUS e')
                return (e == expected)   

        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do  

    describe "Extension tests for ?>" $ do  
        prop "converts a string" $ do
            \(s :: [Char]) -> ioProperty $ do
                e <- s Z.?> ()
                e' <- Z.toExp s
                expected <- Z.createExp (Z.OPTIONAL e')
                return (e == expected)
   
        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do
    describe "Extension tests for ^>{n}" $ do 
        prop "converts a string" $ do
            \n (s :: [Char]) -> ioProperty $ do
                e <- s Z.^> n
                e' <- Z.toExp s
                expected <- Z.createExp (Z.NTIMES n e')
                return (e == expected)    

        -- | TODO: create arbitrary Exp types for RexpZipperv2 
        -- prop "converts an Exp'" $ do
        -- prop "converts an Exp" $ do
        
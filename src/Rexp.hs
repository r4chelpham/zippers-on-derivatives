{-# HLINT ignore "Use foldl" #-}
{-# LANGUAGE FlexibleInstances #-}

module Rexp where

import qualified Data.Set as Set
import Test.QuickCheck

data Rexp = ZERO
            | ONE
            | CHAR Char
            | ALT Rexp Rexp
            | SEQ Rexp Rexp
            | STAR Rexp
            | RECD String Rexp
            | RANGE (Set.Set Char)
            | PLUS Rexp
            | OPTIONAL Rexp
            | NTIMES Rexp Int deriving (Show, Eq)

-- We define an Arbitrary instance of Rexp for property testing purposes.
instance Arbitrary Rexp where
  arbitrary = oneof [ return ZERO
                    , return ONE
                    , CHAR <$> arbitrary
                    , ALT <$> arbitrary <*> arbitrary
                    , SEQ <$> arbitrary <*> arbitrary
                    , STAR <$> arbitrary
                    , OPTIONAL <$> arbitrary
                    , PLUS <$> arbitrary
                    , NTIMES <$> arbitrary <*> choose (0, 100)
                    ]


nullable :: Rexp -> Bool
nullable ZERO = False
nullable ONE = True
nullable (CHAR _) = False
nullable (ALT r1 r2) = nullable r1 || nullable r2
nullable (SEQ r1 r2) = nullable r1 && nullable r2
nullable (STAR _) = True
nullable (RECD _ r) = nullable r
nullable (RANGE _) = False
nullable (PLUS r) = nullable r
nullable (OPTIONAL _) = True
nullable (NTIMES _ 0) = True
nullable (NTIMES r _) = nullable r

der :: Rexp -> Char -> Rexp
der ZERO _ = ZERO
der ONE _ = ZERO
der (CHAR c) d = if c == d then ONE else ZERO
der (ALT r1 r2) c = ALT (der r1 c) (der r2 c)
der (SEQ r1 r2) c =  if nullable r1 then ALT (SEQ (der r1 c) r2) (der r2 c) else SEQ (der r1 c) r2
der r@(STAR r1) c = SEQ (der r1 c) r
der (RECD _ r) c = der r c
der (RANGE cs) c = if Set.member c cs then ONE else ZERO
der (PLUS r) c = SEQ (der r c) (STAR r)
der (OPTIONAL r) c = der r c
der (NTIMES _ 0) _ = ZERO
der (NTIMES r n) c = SEQ (der r c) (NTIMES r (n-1))

ders :: Rexp -> [Char] -> Rexp
ders r [] = r
ders r (c:cs) = ders (der r c) cs

matcher :: Rexp -> [Char] -> Bool
matcher r s = nullable (ders r s)

-- helper functions for converting strings to regular expressions
charsToRexp :: [Char] -> Rexp
charsToRexp [] = ONE
charsToRexp [c] = CHAR c
charsToRexp (c:cs) = SEQ (CHAR c) (charsToRexp cs)

class ToRexp a where
    toRexp :: a -> Rexp

instance ToRexp [Char] where
    toRexp = charsToRexp

infixl 5 ~
infixl 4 +
-- infixl 6 %

(~) :: ToRexp a => a -> Rexp -> Rexp
x ~ r = SEQ (toRexp x) r

-- (%) :: ToRexp a => a -> Rexp
-- x % = STAR (toRexp x)

(+) :: ToRexp a => a -> Rexp -> Rexp
x + r = ALT (toRexp x) r

(#) :: String -> Rexp -> Rexp
s # r = RECD s r
{-# HLINT ignore "Use foldl" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module RexpBench where

import Data.Function.Memoize
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
            | NTIMES Rexp Int 
            deriving (Show, Eq, Ord) 

instance Memoizable (Set.Set Char) where
  memoize f s = memoize (\cs -> f (Set.fromList cs)) (Set.toAscList s)

deriveMemoizable ''Rexp
-- We define an Arbitrary instance of Rexp for property testing purposes.
instance Arbitrary Rexp where
  arbitrary = oneof [ return ZERO
                    , return ONE
                    , CHAR Prelude.<$> arbitrary
                    , ALT Prelude.<$> arbitrary Prelude.<*> arbitrary
                    , SEQ Prelude.<$> arbitrary Prelude.<*> arbitrary
                    , STAR Prelude.<$> arbitrary
                    , OPTIONAL Prelude.<$> arbitrary
                    , PLUS Prelude.<$> arbitrary
                    , NTIMES Prelude.<$> arbitrary <*> choose (0, 100)
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

der :: Rexp -> Char -> Int -> (Rexp, Int)
der ZERO _ count = (ZERO, count + 1)
der ONE _ count = (ZERO, count + 1)
der (CHAR c) d count = 
  if c == d then (ONE, count + 1) else (ZERO, count + 1)
der (ALT r1 r2) c count = 
  let (r1', c1) = der r1 c (count + 1)
      (r2', c2) = der r2 c (c1 + 1)
  in (ALT r1' r2', c2 + 1)
der (SEQ r1 r2) c count = 
  if nullable r1 then 
    let (r1', c1) = der r1 c (count + 1)
        (r2', c2) = der r2 c (c1 + 1)
    in (ALT (SEQ r1' r2) r2', c2 + 1) 
  else 
    let (r1', c1) = der r1 c (count + 1)
    in (SEQ r1' r2, c1 + 1)
der r@(STAR r1) c count = 
  let (r1', c1) = der r1 c (count + 1)
  in (SEQ r1' r, c1 + 1)
der (RECD _ r) c count = der r c (count + 1)
der (RANGE cs) c count = if Set.member c cs then (ONE, count + 1) else (ZERO, count + 1)
der (PLUS r) c count = 
  let (r', c1) = der r c (count + 1)
  in (SEQ r' (STAR r), c1 + 1)
der (OPTIONAL r) c count = der r c (count + 1)
der (NTIMES _ 0) _ count = (ZERO, count + 1)
der (NTIMES r n) c count = 
  let (r', c1) = der r c (count + 1)
  in (SEQ r' (NTIMES r (n-1)), c1 + 1)

ders :: Rexp -> [Char] -> Int -> (Rexp, Int)
ders r [] count = (r, count)
ders r (c:cs) count = 
  let (r', count') = der r c count
  in ders r' cs count'

run :: Rexp -> [Char] -> (Rexp, Int)
run r s = ders r s 0

matcher :: Rexp -> [Char] -> Bool
matcher r s = 
  let (r', _) = run r s
  in nullable r'

simp :: Rexp -> Rexp
simp (ALT r1 r2) =
    let r1s = simp r1
        r2s = simp r2
    in case (r1s, r2s) of
        (ZERO, _) -> r2s
        (_, ZERO) -> r1s
        _ ->
            if r1s == r2s then
                r1s
            else
                ALT r1s r2s
simp (SEQ r1 r2) =
    let r1s = simp r1
        r2s = simp r2
    in case (r1s, r2s) of
        (ZERO, _) -> ZERO
        (_, ZERO) -> ZERO
        (ONE, _) -> r2s
        (_, ONE) -> r1s
        _ -> SEQ r1s r2s
simp r = r

hasFirst :: Rexp -> Bool
hasFirst (CHAR _) = True
hasFirst (ALT r1 r2) = hasFirst r1 || hasFirst r2
hasFirst (SEQ r1 r2) = (hasFirst r1 && isProductive r2) || (nullable r1 && hasFirst r2)
hasFirst (STAR r) = hasFirst r 
hasFirst (PLUS r) = hasFirst r
hasFirst (OPTIONAL r) = hasFirst r
hasFirst (RANGE _) = True 
hasFirst (NTIMES r n) = hasFirst r 
hasFirst _ = False

isProductive :: Rexp -> Bool
isProductive r = nullable r || hasFirst r 

stringToRexp :: [Char] -> Rexp
stringToRexp [] = ONE
stringToRexp [c] = CHAR c
stringToRexp (c:cs) = SEQ (CHAR c) (stringToRexp cs)

class ToRexp a where
  toRexp :: a -> Rexp

instance ToRexp Rexp where
  toRexp :: Rexp -> Rexp
  toRexp = id 

instance ToRexp String where
  toRexp = stringToRexp

infixl 9 ^>
infixl 8 ?>
infixl 7 +>
infixl 6 *>
infixl 4 <~>
infixl 3 <|>
infixl 1 <$>

(<~>) :: (ToRexp a, ToRexp b) => a -> b -> Rexp
a <~> b = SEQ (toRexp a) (toRexp b)

(<|>) :: (ToRexp a, ToRexp b) => a -> b -> Rexp
a <|> b = ALT (toRexp a) (toRexp b)

(<$>) :: String -> Rexp -> Rexp
s <$> r = RECD s r

(*>) :: ToRexp a => a -> b -> Rexp
r *> _ = STAR (toRexp r)

(+>) :: ToRexp a => a -> b -> Rexp
r +> _ = PLUS (toRexp r)

(?>) :: ToRexp a => a -> b -> Rexp
r ?> _ = OPTIONAL (toRexp r)

(^>) :: ToRexp a => a -> Int -> Rexp
r ^> n = NTIMES (toRexp r) n

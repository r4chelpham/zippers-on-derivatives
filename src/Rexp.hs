{-# HLINT ignore "Use foldl" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Rexp where

import Data.Function.Memoize
import qualified Data.Set as Set

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
ders r (c:cs) = ders (simp (der r c)) cs

matcher :: Rexp -> [Char] -> Bool
matcher r s = nullable (ders r s)

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

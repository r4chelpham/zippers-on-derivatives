module Rexp (
    Rexp
) where

import qualified Data.Set as Set

data Rexp = ZERO
            | ONE
            | CHAR Char
            | ALT Rexp Rexp
            | SEQ Rexp Rexp
            | STAR Rexp
            | RANGE (Set.Set Char)
            | PLUS Rexp
            | OPTIONAL Rexp
            | NTIMES Rexp Int
            | RECD String Rexp
            | CFUN (Char -> Bool)

nullable :: Rexp -> Bool
nullable ZERO = False
nullable ONE = True
nullable (CHAR _) = False
nullable (ALT r1 r2) = nullable r1 || nullable r2
nullable (SEQ r1 r2) = nullable r1 && nullable r2
nullable (STAR _) = True
nullable (RANGE _) = False
nullable (PLUS r) = nullable r
nullable (OPTIONAL _) = True
nullable (NTIMES _ 0) = True
nullable (NTIMES r _) = nullable r
nullable (RECD _ r) = nullable r
nullable (CFUN _) = False

der :: Rexp -> Char -> Rexp
der ZERO _ = ZERO
der ONE _ = ZERO
der (CHAR c) d = if c == d then ONE else ZERO
der (ALT r1 r2) c = ALT (der r1 c) (der r2 c)
der (SEQ r1 r2) c =  if nullable r1 then ALT (SEQ (der r1 c) r2) (der r2 c) else SEQ (der r1 c) r2
der (STAR r) c = SEQ (der r c) r
der (RANGE cs) c = if Set.member c cs then ONE else ZERO
der (PLUS r) c = SEQ (der r c) (STAR r)
der (OPTIONAL r) c = der r c
der (NTIMES _ 0) _ = ZERO
der (NTIMES r n) c = SEQ (der r c) (NTIMES r (n-1))
der (RECD _ r) c = der r c
der (CFUN f) c
    | f c = ONE
    | otherwise = ZERO

module EdelmannZipper where

import Rexp
import qualified Data.Set as Set

type Context = [Rexp]
type Zipper = Set.Set Context

focus :: Rexp -> Zipper
focus r = Set.singleton [r]

up :: Char -> Context -> Zipper
up _ [] = Set.empty
up c (right:parent)
    | nullable right = Set.union (down right c parent) (up c parent)
    | otherwise = down right c parent

down :: Rexp -> Char -> Context -> Zipper
down ZERO _ _ = Set.empty
down ONE _ _ = Set.empty
down (CHAR d) c ctx
    | c == d = Set.singleton ctx
    | otherwise = Set.empty
down (ALT r1 r2) c ctx = 
    Set.union (down r1 c ctx) (down r2 c ctx)
down (SEQ r1 r2) c ctx 
    | nullable r1 = 
        Set.union (down r1 c (r2:ctx)) (down r2 c ctx)
    | otherwise = down r1 c (r2:ctx)
down r@(STAR r1) c ctx = down r1 c (r:ctx)
down (OPTIONAL r1) c ctx = down r1 c ctx
down (RANGE cs) c ctx
    | Set.member c cs = Set.singleton ctx
    | otherwise = Set.empty
down (PLUS r1) c ctx = down r1 c (STAR r1:ctx)
down (NTIMES _ 0) _ _ = Set.empty
down (NTIMES r1 n) c ctx = down r1 c (NTIMES r1 (n-1):ctx)
down (RECD _ r1) c ctx = down r1 c ctx

der :: Zipper -> Char -> Zipper
der z c = Set.unions $ Set.map (up c) z

ders :: Zipper -> [Char] -> Zipper
ders z [] = z
ders z (c:cs) = EdelmannZipper.ders (EdelmannZipper.der z c) cs

matcher :: Rexp -> [Char] -> Bool
matcher r s = any isNullable (Set.toList (EdelmannZipper.ders (focus r) s))
  where
    isNullable :: Context -> Bool
    isNullable [] = True
    isNullable ctx = all nullable ctx
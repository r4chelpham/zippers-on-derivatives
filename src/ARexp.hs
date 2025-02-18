{- 
    Implementation of regular expressions annotated with bitcodes
-}
module ARexp where

import Rexp
import Val
import qualified Data.Set as Set

data Bit = S | Z deriving (Show, Eq, Ord)

data ARexp = AZERO
            | AONE [Bit]
            | ACHAR Char [Bit]
            | AALT [ARexp] [Bit]
            | ASEQ ARexp ARexp [Bit]
            | ASTAR ARexp [Bit] 
            | ARANGE (Set.Set Char) [Bit] deriving (Show, Eq, Ord)

fuse :: [Bit] -> ARexp -> ARexp
fuse _ AZERO = AZERO
fuse bs (AONE bs') = AONE (bs++bs')
fuse bs (ACHAR c bs') = ACHAR c (bs++bs')
fuse bs (AALT as bs') = AALT as (bs++bs')
fuse bs (ASEQ a1 a2 bs') = ASEQ a1 a2 (bs++bs')
fuse bs (ASTAR a bs') = ASTAR a (bs++bs')
fuse bs (ARANGE cs bs') = ARANGE cs (bs++bs')

internalise :: Rexp -> ARexp
internalise ZERO = AZERO
internalise ONE = AONE []
internalise (CHAR c) = ACHAR c []
internalise (SEQ r1 r2) = ASEQ (internalise r1) (internalise r2) [] 
internalise (ALT r1 r2) = AALT [fuse [Z] (internalise r1), fuse [S] (internalise r2)] []
internalise (STAR r1) = ASTAR (internalise r1) []
internalise (RANGE cs) = ARANGE cs []

erase :: ARexp -> Rexp
erase AZERO = ZERO
erase (AONE _) = ONE
erase (ACHAR c _) = CHAR c 
erase (ASEQ a1 a2 _) = SEQ (erase a1) (erase a2)
erase (AALT [a1, a2] _) = ALT (erase a1) (erase a2)
erase (AALT (a:as) _) = ALT (erase a) (erase (AALT as []))
erase (ASTAR a _) = STAR (erase a)
erase (ARANGE cs _) = RANGE cs

bnullable :: ARexp -> Bool
bnullable a = nullable (erase a) 

bmkeps :: ARexp -> [Bit]
bmkeps (AONE bs) = bs
bmkeps (AALT (a:as) bs)
    | bnullable a = bs ++ bmkeps a
    | otherwise = bs ++ bmkeps (AALT as [])
bmkeps (ASEQ a1 a2 bs) = bs ++ bmkeps a1 ++ bmkeps a2
bmkeps (ASTAR a bs) = bs ++ [S]

decode' :: [Bit] -> Rexp -> (Val, [Bit])
decode' bs ONE = (Empty, bs)
decode' bs (CHAR c) = (Chr c, bs)
decode' (Z:bs) (ALT r1 r2) = 
    let (v, bs1) = decode' bs r1 in
        (Val.Left v, bs1)
decode' (S:bs) (ALT r1 r2) = 
    let (v, bs1) = decode' bs r2 in
        (Val.Right v, bs1)
decode' bs (SEQ r1 r2) = 
    let (v1, bs1) = decode' bs r1
        (v2, bs2) = decode' bs1 r2 in
        (Sequ v1 v2, bs2)
decode' (S:bs) (STAR r) = (Stars [], bs)
decode' (Z:bs) (STAR r) = 
    let (v, bs1) = decode' bs r
        (Stars vs, bs2) = decode' bs1 (STAR r) in
            (Stars (v:vs), bs2)
decode' bs (RANGE cs) = (Chr 'c', bs)
decode' _ _ = (Empty, [S])

decode :: [Bit] -> Rexp -> Maybe Val
decode bs r = 
    let (v, bs') = decode' bs r in
        if null bs' then Just v else Nothing

code :: Val -> [Bit]
code Empty = []
code (Chr c) = []
code (Val.Left v) = Z:code v
code (Val.Right v) = S:code v
code (Sequ v1 v2) = code v1 ++ code v2
code (Stars []) = [S]
code (Stars (v:vs)) = Z:(code v ++ code (Stars vs))

bder :: ARexp -> Char -> ARexp
bder AZERO c = AZERO
bder (AONE bs) c = AZERO
bder (ACHAR d bs) c = if c == d then AONE bs else AZERO
bder (AALT as bs) c = AALT (map (`bder` c) as) bs
bder (ASEQ a1 a2 bs) c
    | bnullable a1 = AALT [ASEQ (bder a1 c) a2 [], fuse (bmkeps a1) (bder a2 c)] bs
    | otherwise = ASEQ (bder a1 c) a2 bs
bder (ASTAR a bs) c = ASEQ (bder (fuse [Z] a) c) (ASTAR a []) bs
bder (ARANGE cs bs) c = if Set.member c cs then AONE bs else AZERO

bders :: ARexp -> [Char] -> ARexp
bders a [] = a
bders a (c:cs) = bders (bder a c) cs

blexer :: Rexp -> [Char] -> Maybe Val
blexer r s = 
    let a = bders (internalise r) s in 
        if bnullable a then decode (bmkeps a) r else Nothing

{- 
    Implementation of regular expressions annotated with bitcodes
-}
module ARexp where
import Rexp
import Val
import qualified Data.Set as Set

data Bit = S | Z | C Char deriving (Show, Eq, Ord)

data ARexp = AZERO
            | AONE [Bit]
            | ACHAR Char [Bit]
            | AALT [ARexp] [Bit]
            | ASEQ ARexp ARexp [Bit]
            | ASTAR ARexp [Bit] 
            | ARANGE (Set.Set Char) [Bit] 
            | AOPTIONAL ARexp [Bit] 
            | APLUS ARexp [Bit] 
            | ANTIMES ARexp Int [Bit] 
            | ARECD String ARexp [Bit] deriving (Show, Eq, Ord)

fuse :: [Bit] -> ARexp -> ARexp
fuse _ AZERO = AZERO
fuse bs (AONE bs') = AONE (bs++bs')
fuse bs (ACHAR c bs') = ACHAR c (bs++bs')
fuse bs (AALT as bs') = AALT as (bs++bs')
fuse bs (ASEQ a1 a2 bs') = ASEQ a1 a2 (bs++bs')
fuse bs (ASTAR a bs') = ASTAR a (bs++bs')
fuse bs (ARANGE cs bs') = ARANGE cs (bs++bs')
fuse bs (AOPTIONAL a bs') = AOPTIONAL a (bs++bs')
fuse bs (APLUS a bs') = APLUS a (bs++bs')
fuse bs (ANTIMES a n bs') = ANTIMES a n (bs++bs')
fuse bs (ARECD s a bs') = ARECD s a (bs++bs')

appendBits :: [Bit] -> ARexp -> ARexp
appendBits _ AZERO = AZERO
appendBits bs (AONE bs') = AONE (bs'++bs)
appendBits bs (ACHAR c bs') = ACHAR c (bs'++bs)
appendBits bs (AALT as bs') = AALT as (bs'++bs)
appendBits bs (ASEQ a1 a2 bs') = ASEQ a1 a2 (bs'++bs)
appendBits bs (ASTAR a bs') = ASTAR a (bs'++bs)
appendBits bs (ARANGE cs bs') = ARANGE cs (bs'++bs)
appendBits bs (AOPTIONAL a bs') = AOPTIONAL a (bs'++bs)
appendBits bs (APLUS a bs') = APLUS a (bs'++bs)
appendBits bs (ANTIMES a n bs') = ANTIMES a n (bs'++bs)
appendBits bs (ARECD s a bs') = ARECD s a (bs'++bs)

internalise :: Rexp -> ARexp
internalise ZERO = AZERO
internalise ONE = AONE []
internalise (CHAR c) = ACHAR c []
internalise (SEQ r1 r2) = ASEQ (internalise r1) (internalise r2) [] 
internalise (ALT r1 r2) = AALT [fuse [Z] (internalise r1), fuse [S] (internalise r2)] []
internalise (STAR r1) = ASTAR (internalise r1) []
internalise (RANGE cs) = ARANGE cs []
internalise (OPTIONAL r) = AOPTIONAL (internalise r) []
internalise (PLUS r) = APLUS (internalise r) []
internalise (NTIMES r n) = ANTIMES (internalise r) n []
internalise (RECD s r) = ARECD s (internalise r) []

erase :: ARexp -> Rexp
erase AZERO = ZERO
erase (AONE _) = ONE
erase (ACHAR c _) = CHAR c 
erase (ASEQ a1 a2 _) = SEQ (erase a1) (erase a2)
erase (AALT [a1, a2] _) = ALT (erase a1) (erase a2)
erase (AALT (a:as) _) = ALT (erase a) (erase (AALT as []))
erase (ASTAR a _) = STAR (erase a)
erase (ARANGE cs _) = RANGE cs
erase (AOPTIONAL a _) = OPTIONAL (erase a)
erase (APLUS a _) = PLUS (erase a)
erase (ANTIMES a n _) = NTIMES (erase a) n
erase (ARECD s a _) = RECD s (erase a)

bnullable :: ARexp -> Bool
bnullable a = nullable (erase a) 

bmkeps :: ARexp -> [Bit]
bmkeps (AONE bs) = bs
bmkeps (AALT (a:as) bs)
    | bnullable a = bs ++ bmkeps a
    | otherwise = bs ++ bmkeps (AALT as [])
bmkeps (ASEQ a1 a2 bs) = bs ++ bmkeps a1 ++ bmkeps a2
bmkeps (ASTAR a bs) = bs ++ [S]
bmkeps (AOPTIONAL a bs) = bs
bmkeps (APLUS a bs) = bs ++ [S]
bmkeps (ANTIMES a n bs) = bs ++ [S]
bmkeps (ARECD s a bs) = bs

decode' :: [Bit] -> Rexp -> (Val, [Bit])
decode' bs ONE = (Empty, bs)
decode' bs (CHAR c) = (Chr c, bs)
decode' (Z:bs) (ALT r1 _) = 
    let (v, bs1) = decode' bs r1 in
        (Val.Left v, bs1)
decode' (S:bs) (ALT _ r2) = 
    let (v, bs1) = decode' bs r2 in
        (Val.Right v, bs1)
decode' bs (SEQ r1 r2) = 
    let (v1, bs1) = decode' bs r1
        (v2, bs2) = decode' bs1 r2 in
        (Sequ v1 v2, bs2)
decode' (S:bs) (STAR _) = (Stars [], bs)
decode' (Z:bs) r@(STAR r') = 
    let (v, bs1) = decode' bs r'
        (v', bs2) = decode' bs1 r in
        case v' of
            Stars vs -> (Stars (v:vs), bs2)
            _ -> (Empty, [S])
decode' (C c:bs) (RANGE _) = (Chr c, bs)
decode' bs (OPTIONAL r) = 
    let (v, bs1) = decode' bs r in
        (Opt v, bs1)
decode' (S:bs) (PLUS _) = (Pls [], bs)
decode' (Z:bs) r@(PLUS r') =
    let (v, bs1) = decode' bs r'
        (Pls vs, bs2) = decode' bs1 r in
            (Pls (v:vs), bs2)
decode' (S:bs) (NTIMES _ 0) = (NX [], bs)
decode' (Z:bs) (NTIMES r' n) = 
    let (v, bs1) = decode' bs r'
        (v', bs2) = decode' bs1 (NTIMES r' (n-1)) in
            case v' of 
                NX vs -> (NX (v:vs), bs2)
                _ -> (Empty, [S])
decode' bs (RECD s r) =
    let (v, bs1) = decode' bs r in
        (Rec s v, bs1)
decode' _ _ = (Empty, [S])

decode :: [Bit] -> Rexp -> Maybe Val
decode bs r = 
    let (v, bs') = decode' bs r in
        if null bs' then Just v else Nothing

code :: Val -> [Bit]
code Empty = []
code (Chr _) = []
code (Val.Left v) = Z:code v
code (Val.Right v) = S:code v
code (Sequ v1 v2) = code v1 ++ code v2
code (Stars []) = [S]
code (Stars (v:vs)) = Z:(code v ++ code (Stars vs))
code (Opt v) = code v
code (Pls (v:vs)) = Z:(code v ++ code (Pls vs))
code (NX []) = [S]
code (NX (v:vs)) = Z:(code v ++ code (NX vs))
code (Rec _ v) = code v

bder :: ARexp -> Char -> ARexp
bder AZERO _ = AZERO
bder (AONE _) _ = AZERO
bder (ACHAR d bs) c = if c == d then AONE bs else AZERO
bder (AALT as bs) c = AALT (map (`bder` c) as) bs
bder (ASEQ a1 a2 bs) c
    | bnullable a1 = AALT [ASEQ (bder a1 c) a2 [], fuse (bmkeps a1) (bder a2 c)] bs
    | otherwise = ASEQ (bder a1 c) a2 bs
bder (ASTAR a bs) c = ASEQ (bder (fuse [Z] a) c) (ASTAR a []) bs
bder (AOPTIONAL a _) c = bder a c
bder (ARANGE cs bs) c = if Set.member c cs then AONE (bs++[C c]) else AZERO
bder (APLUS a bs) c = ASEQ (bder (fuse [Z] a) c) (ASTAR a []) bs
bder (ANTIMES a 0 bs) c = AZERO
bder (ANTIMES a n bs) c = ASEQ (bder (fuse [Z] a) c) (ANTIMES a (n-1) []) bs
bder (ARECD _ a bs) c = bder (fuse bs a) c

bders :: ARexp -> [Char] -> ARexp
bders a [] = a
bders a (c:cs) = bders (bder a c) cs

blexer :: Rexp -> [Char] -> Maybe Val
blexer r s = 
    let a = bders (internalise r) s in 
        if bnullable a then decode (bmkeps a) r else Nothing

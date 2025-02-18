module EdelmannZipperv2 where

import Rexp
import ARexp
import Val
import qualified Data.Set as Set
import Data.Char (intToDigit)
import Data.List (intercalate)

type Context = [ARexp]
type Zipper = Set.Set Context

focus :: Rexp -> Zipper
focus r = Set.singleton [internalise r]

extract :: ARexp -> [Bit]
extract AZERO = []
extract (AONE bs) = bs
extract (ACHAR _ bs) = bs
extract (ASEQ _ _ bs) = bs
extract (AALT _ bs) = bs
extract (ASTAR _ bs) = bs
extract (ARANGE _ bs) = bs

uncontext :: Context -> ARexp
uncontext [] = AONE []
uncontext [ct] = ct
uncontext (ct:cts) = ASEQ ct (uncontext cts) []

unfocus :: Zipper -> [ARexp]
unfocus z = map uncontext (Set.toList z)

up :: Char -> Context -> Zipper
up _ [] = Set.empty
up c (right:parent)
    | bnullable right = Set.union (down right c parent) (up c parent)
    | otherwise = down right c parent

down :: ARexp -> Char -> Context -> Zipper
down AZERO _ _ = Set.empty
down (AONE _) _ _ = Set.empty
down (ACHAR d bs) c ctx
    | c == d = 
        case ctx of 
            [] -> Set.singleton [AONE bs]
            ct -> Set.singleton (map (fuse bs) ctx)
    | otherwise = Set.empty
down (AALT rs bs) c ctx = 
    Set.unions (map (\r -> down (fuse bs r) c ctx) rs)
down (ASEQ r1 r2 bs) c ctx 
    | bnullable r1 = 
        Set.union (down r1 c (r2:ctx)) (down (fuse (bmkeps r1++bs) r2) c ctx)
    | otherwise = down r1 c (fuse bs r2:ctx)
down r@(ASTAR r1 bs) c ctx = down (fuse (bs++[Z]) r1) c (ASTAR r1 []:ctx)
-- down (OPTIONAL r1) c ctx = down r1 c ctx
down (ARANGE cs bs) c ctx
    | Set.member c cs = 
        case ctx of 
            [] -> Set.singleton [AONE bs]
            ct -> Set.singleton (map (fuse bs) ctx)
    | otherwise = Set.empty
-- down (PLUS r1) c ctx = down r1 c (STAR r1:ctx)
-- down (NTIMES _ 0) _ _ = Set.empty
-- down (NTIMES r1 n) c ctx = down r1 c (NTIMES r1 (n-1):ctx)
-- down (RECD _ r1) c ctx = down r1 c ctx

zder :: Zipper -> Char -> Zipper
zder z c = Set.unions $ Set.map (up c) z

zders :: Zipper -> [Char] -> Zipper
zders z [] = z
zders z (c:cs) = zders (zder z c) cs

isNullable :: Zipper -> Bool
isNullable z = any nullableCtx (Set.toList z)
    where
    nullableCtx :: Context -> Bool
    nullableCtx [] = True
    nullableCtx ctx = all bnullable ctx

matcher :: Rexp -> [Char] -> Bool
matcher r s = isNullable (zders (focus r) s)

-- zblexer :: Rexp -> [Char] -> [Maybe Val]
-- zblexer r s =
--     let zs = zders (focus r) s
--         nullzs = filter (all bnullable) (Set.toList zs) in
--             map (\a -> decode (bmkeps a) r) nullzs

-- -- pretty-printing REGs
-- implode :: [[Char]] -> [Char]
-- implode = intercalate "\n"

-- explode :: [Char] -> [[Char]]
-- explode = lines

-- lst :: [Char] -> [Char]
-- lst s = case explode s of
--     []   -> ""
--     h:tl -> implode $ (" └" ++ h) : map ("  " ++) tl

-- mid :: [Char] -> [Char]
-- mid s = case explode s of
--     []   -> ""
--     h:tl -> implode $ (" ├" ++ h) : map (" │" ++) tl

-- indent :: [[Char]] -> [Char]
-- indent [] = ""
-- indent ss = implode $ map mid (init ss) ++ [lst (last ss)]

-- pps :: [Rexp] -> String
-- pps es = indent (map pp es)

-- pp :: Rexp -> String
-- pp ZERO        = "0\n"
-- pp ONE         = "1\n"
-- pp (CHAR c)     = c : "\n"
-- pp (RANGE cs)     = Set.showTreeWith True False cs ++ "\n"
-- pp (SEQ r1 r2)  =  "SEQ\n" ++ pps [r1, r2]
-- pp (ALT r1 r2) = "ALT\n" ++ pps [r1, r2]
-- pp (STAR r)    = "STAR\n" ++ pps [r]
-- pp (OPTIONAL r)    = "OPTIONAL\n" ++ pps [r]
-- pp (PLUS r)    = "PLUS\n" ++ pps [r]
-- pp (NTIMES r n)    = "NTIMES\n" ++ [intToDigit n] ++ "\n" ++ pps [r]
-- pp (RECD s r)    = "RECD\n" ++ s ++ "\n" ++ pps [r]

-- ppz :: Zipper -> String
-- ppz z = "ZIP\n" ++ indent (map ppctx (Set.toList z))

-- ppctx :: Context -> String
-- ppctx ct = indent (map pp ct)

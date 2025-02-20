module EdelmannZipperv2 where

import Rexp
import ARexp
import Val
import qualified Data.Set as Set
import Data.Char (intToDigit)
import Data.List (intercalate, minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

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
extract (ASTAR _ bs) = bs ++ [S]
extract (AOPTIONAL _ bs) = bs
extract (APLUS _ bs) = bs ++ [S]
extract (ANTIMES _ 0 bs) = bs ++ [S]
extract (ANTIMES {}) = []
extract (ARANGE _ bs) = bs
extract (ARECD _ _ bs) = bs

uncontext :: Context -> ARexp
uncontext [] = AONE []
uncontext [ct] = ct
uncontext (ct:cts) = ASEQ ct (uncontext cts) []

unfocus :: Zipper -> [ARexp]
unfocus z = map uncontext (Set.toList z)

up :: Char -> Context -> Zipper
up _ [] = Set.empty
up c (right:parent)
    | bnullable right =
        let bs = extract right in
            Set.union (down right c parent) (up c (map (fuse bs) parent))
    | otherwise = down right c parent

down :: ARexp -> Char -> Context -> Zipper
down AZERO _ _ = Set.empty
down (AONE _) _ _ = Set.empty
down (ACHAR d bs) c ctx
    | c == d =
        case ctx of
            [] -> Set.singleton [AONE bs]
            _ -> Set.singleton (map (fuse bs) ctx)
    | otherwise = Set.empty
down (AALT rs bs) c ctx =
    Set.unions (map (\r -> down (fuse bs r) c ctx) rs)
down (ASEQ r1 r2 bs) c ctx
    | bnullable r1 =
        Set.union (down r1 c (fuse bs r2:ctx)) (down (fuse (bs++bmkeps r1) r2) c ctx)
    | otherwise = down r1 c (fuse bs r2:ctx)
down (ASTAR r1 bs) c ctx = down (fuse (bs++[Z]) r1) c (ASTAR r1 []:ctx)
down (AOPTIONAL r1 bs) c ctx = down (fuse bs r1) c ctx
down (ARANGE cs bs) c ctx
    | Set.member c cs =
        case ctx of
            [] -> Set.singleton [AONE (bs++[C c])]
            _ -> Set.singleton (map (appendBits (bs++[C c])) ctx)
    | otherwise = Set.empty
down (APLUS r1 bs) c ctx = down (fuse (bs++[Z]) r1) c (ASTAR r1 []:ctx)
down (ANTIMES _ 0 _) _ _ = Set.empty
down (ANTIMES r1 n bs) c ctx = down (fuse (bs++[Z]) r1) c (ANTIMES r1 (n-1) []:ctx)
down (ARECD _ r1 bs) c ctx = down (fuse bs r1) c ctx

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

extractVal :: Maybe Val -> Val
extractVal = fromMaybe Empty

shortestList :: [[t]] -> [t]
shortestList = minimumBy (comparing length)

zblexer :: Rexp -> [Char] -> Val
zblexer r s =
    let zs = zders (focus r) s
        a = shortestList (map bmkeps (filter bnullable (unfocus zs)))
        v = decode a r in
            extractVal v

-- -- pretty-printing REGs
implode :: [[Char]] -> [Char]
implode = intercalate "\n"

explode :: [Char] -> [[Char]]
explode = lines

lst :: [Char] -> [Char]
lst s = case explode s of
    []   -> ""
    h:tl -> implode $ (" └" ++ h) : map ("  " ++) tl

mid :: [Char] -> [Char]
mid s = case explode s of
    []   -> ""
    h:tl -> implode $ (" ├" ++ h) : map (" │" ++) tl

indent :: [[Char]] -> [Char]
indent [] = ""
indent ss = implode $ map mid (init ss) ++ [lst (last ss)]

pps :: [ARexp] -> String
pps es = indent (map pp es)

ppb :: [Bit] -> [Char]
ppb [] = []
ppb (S:bs) = 'S':ppb bs
ppb (Z:bs) = 'Z':ppb bs
ppb (C c:bs) = ('C':[c])++ppb bs

pp :: ARexp -> String
pp AZERO        = "0\n"
pp (AONE bs)        = "1\n" ++ ppb bs ++ "\n"
pp (ACHAR c bs)     = c : "\n" ++ ppb bs ++ "\n"
pp (ARANGE cs bs)     = Set.showTreeWith True False cs ++ "\n" ++ ppb bs ++ "\n"
pp (ASEQ r1 r2 bs)  =  "SEQ\n" ++ pps [r1, r2] ++ ppb bs ++ "\n"
pp (AALT rs bs) = "ALT\n" ++ pps rs ++ "\n" ++ ppb bs ++ "\n"
pp (ASTAR r bs)    = "STAR\n" ++ pps [r] ++ ppb bs ++ "\n"
pp (AOPTIONAL r bs)    = "OPTIONAL\n" ++ pps [r] ++ ppb bs ++ "\n"
pp (APLUS r bs)    = "PLUS\n" ++ pps [r] ++ ppb bs ++ "\n"
pp (ANTIMES r n bs)    = "NTIMES\n" ++ [intToDigit n] ++ "\n" ++ pps [r] ++ ppb bs ++ "\n"
pp (ARECD s r bs)    = "RECD\n" ++ s ++ "\n" ++ pps [r] ++ ppb bs ++ "\n"

ppz :: Zipper -> String
ppz z = "ZIP\n" ++ indent (map ppctx (Set.toList z))

ppctx :: Context -> String
ppctx ct = indent (map pp ct)

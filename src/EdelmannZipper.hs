{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module EdelmannZipper where

import Rexp
import qualified Data.Set as Set
import Data.Function.Memoize
import Data.Char (intToDigit)
import Data.List (intercalate)


type Context = [Rexp]
type Zipper = Set.Set Context

instance Memoizable Zipper where
  memoize f s = memoize (\cs -> f (Set.fromList cs)) (Set.toAscList s)

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

isNullable :: Zipper -> Bool
isNullable = any nullableCtx
    where
    nullableCtx :: Context -> Bool
    nullableCtx [] = True
    nullableCtx ctx = all nullable ctx

matcher :: Rexp -> [Char] -> Bool
matcher r s = isNullable (EdelmannZipper.ders (focus r) s)

hasFirst :: Zipper -> Bool
hasFirst z = any (\c -> any Rexp.hasFirst c && all isProductive c) z


-- pretty-printing Zippers
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

pps :: [Rexp] -> String
pps es = indent (map pp es)

pp :: Rexp -> String
pp ZERO        = "0\n"
pp ONE         = "1\n"
pp (CHAR c)     = c : "\n"
pp (RANGE cs)     = Set.showTreeWith True False cs ++ "\n"
pp (SEQ r1 r2)  =  "SEQ\n" ++ pps [r1, r2]
pp (ALT r1 r2) = "ALT\n" ++ pps [r1, r2]
pp (STAR r)    = "STAR\n" ++ pps [r]
pp (OPTIONAL r)    = "OPTIONAL\n" ++ pps [r]
pp (PLUS r)    = "PLUS\n" ++ pps [r]
pp (NTIMES r n)    = "NTIMES\n" ++ [intToDigit n] ++ "\n" ++ pps [r]
pp (RECD s r)    = "RECD\n" ++ s ++ "\n" ++ pps [r]

ppz :: Zipper -> String
ppz z = "ZIP\n" ++ indent (map ppctx (Set.toList z))

ppctx :: Context -> String
ppctx ct = indent (map pp ct)

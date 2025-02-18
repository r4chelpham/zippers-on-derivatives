module EdelmannZipper where

import Rexp
import qualified Data.Set as Set
import Data.Char (intToDigit)
import Data.List (intercalate)
import qualified Data.Map as Map

type Context = [Rexp]
type Lengths = Map.Map Rexp Int
type Zipper = Set.Set Context

focus :: Rexp -> Zipper
focus r = Set.singleton [r]

extractZipper :: (Zipper, Lengths) -> Zipper
extractZipper (z, _) = z

extractLengths :: (Zipper, Lengths) -> Lengths
extractLengths (_, ls) = ls

precedence :: Rexp -> Int
precedence ZERO = 0
precedence ONE = 1
precedence (CHAR _) = 1
precedence (RANGE _) = 1
precedence (ALT r1 r2) = max (precedence r1) (precedence r2)
precedence (SEQ r1 r2) = precedence r1 + precedence r2
precedence (STAR r1) = max 1 (precedence r1)
precedence (OPTIONAL r1) = max 1 (precedence r1)
precedence (PLUS r1) = precedence r1
precedence (NTIMES r n) = n * precedence r
precedence (RECD _ r) = precedence r

insertLengths :: [Rexp] -> Lengths -> Lengths
insertLengths [] ls = ls
insertLengths [r] ls =
    let lenr = precedence r in
        Map.insert r lenr ls
insertLengths (r:rs) ls =
    let lenr = precedence r
        m = Map.insert r lenr ls in
            insertLengths rs m

up :: Char -> Context -> Lengths -> (Zipper, Lengths)
up _ [] ls = (Set.empty, ls)
up c (right:parent) ls
    | nullable right =
        let (rs, m) = down right c parent ls
            (rs', m') = up c parent ls in
                (Set.union rs rs', Map.union m m')
    | otherwise =
        let (rs, _) = down right c parent ls in
            (rs, ls)

down :: Rexp -> Char -> Context -> Lengths -> (Zipper, Lengths)
down ZERO _ _ ls = (Set.empty, ls)
down ONE _ _ ls = (Set.empty, ls)
down (CHAR d) c ctx ls
    | c == d = (Set.singleton ctx, ls)
    | otherwise = (Set.empty, ls)
down (ALT r1 r2) c ctx ls =
    let m' = insertLengths [r1, r2] ls in
    (
        Set.union
        (extractZipper (down r1 c ctx m'))
        (extractZipper (down r2 c ctx m'))
        , m'
    )
down (SEQ r1 r2) c ctx ls =
    let m' = insertLengths [r1, r2] ls in
    if nullable r1 then
    (
        Set.union
        (extractZipper (down r1 c (r2:ctx) m'))
        (extractZipper (down r2 c ctx m'))
        , m'
    )
    else down r1 c (r2:ctx) m'
down r@(STAR r1) c ctx ls =
    let m' = insertLengths [r1] ls in
    down r1 c (r:ctx) m'
down (OPTIONAL r1) c ctx ls =
    let m' = insertLengths [r1] ls in
        down r1 c ctx m'
down (RANGE cs) c ctx ls
    | Set.member c cs = (Set.singleton ctx, ls)
    | otherwise = (Set.empty, ls)
down (PLUS r1) c ctx ls =
    let m' = insertLengths [r1] ls in
        down r1 c (STAR r1:ctx) m'
down (NTIMES _ 0) _ _ ls = (Set.empty, ls)
down (NTIMES r1 n) c ctx ls =
    let m' = insertLengths [r1] ls in
        down r1 c (NTIMES r1 (n-1):ctx) m'
down (RECD _ r1) c ctx ls =
    let m' = insertLengths [r1] ls in
        down r1 c ctx m'

-- der :: Zipper -> Char -> (Zipper, Lengths)
-- der z c = Set.unions $ Set.map (up c Map.empty) z

-- ders :: Zipper -> [Char] -> Zipper
-- ders z [] = z
-- ders z (c:cs) = EdelmannZipper.ders (EdelmannZipper.der z c) cs

isNullable :: Zipper -> Bool
isNullable z = any nullableCtx (Set.toList z)
    where
    nullableCtx :: Context -> Bool
    nullableCtx [] = True
    nullableCtx ctx = all nullable ctx

-- matcher :: Rexp -> [Char] -> Bool
-- matcher r s = isNullable (EdelmannZipper.ders (focus r) s)

-- pretty-printing REGs
implode :: [String] -> String
implode = intercalate "\n"

explode :: String -> [String]
explode = lines

lst :: String -> String
lst s = case explode s of
    []   -> ""
    h:tl -> implode $ (" └" ++ h) : map ("  " ++) tl

mid :: String -> String
mid s = case explode s of
    []   -> ""
    h:tl -> implode $ (" ├" ++ h) : map (" │" ++) tl

indent :: [String] -> String
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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RexpZipper where
import qualified Data.Set as Set
import Data.List (intercalate)

type Sym = Char

data Rexp = CHAR Char
            | RANGE (Set.Set Char)
            | SEQ Sym [Rexp]
            | ALT [Rexp]
            | STAR Rexp
            | PLUS Rexp
            | OPTIONAL Rexp
            | NTIMES Int Rexp
            | RECD [Char] Rexp
            deriving (Ord, Eq, Show)

{-
    Default constructors for Rexps that remember how 
    they were matched.

    They all start off with having an empty list.
-}

defaultSEQ :: [Rexp] -> Rexp
defaultSEQ = SEQ sBottom

data Context = TopC
            | SeqC Context Sym [Rexp] [Rexp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Context -- Alternate has one context shared between its children
            | StarC Context [Rexp] Rexp
            | OptionalC Context
            | NTimesC Context Int [Rexp] Rexp
            | RecdC Context [Char]
            deriving (Ord, Eq, Show)

data Zipper = Zipper Rexp Context deriving (Ord, Eq, Show)

{- 
    Whether an Rexp is nullable (can match the empty string).
-}
nullable :: Rexp -> Bool
nullable (CHAR _) = False
nullable (RANGE _) = False
nullable (ALT es) = any nullable es
nullable (SEQ _ es) = all nullable es
nullable (STAR _) = True
nullable (PLUS e) = nullable e
nullable (OPTIONAL _) = True
nullable (NTIMES 0 _) = True
nullable (NTIMES _ r) = nullable r
nullable (RECD _ r) = nullable r

cBottom :: Char
cBottom = '\0'

sBottom :: Char
sBottom = '\0'

{-
    Creates a zipper that focuses on 
    the expression.
-}

focus :: Rexp -> Zipper
focus r = Zipper (SEQ sBottom []) (SeqC TopC sBottom [] [r, CHAR cBottom])

{-
    Derives the result of an Rexp from one character:
    The semantic derivative on a language L wrt a character c
    is as follows: Der L c = { s | c:s is in L }

    Hence the result of taking the successful derivative on 
    a zipper is a zipper that focuses on c and remembers 
    what was around it. 

    It has two functions:
    up - moves back up the zipper via its given Context 
    (the parent it can traverse upwards from)
    down - moves down the zipper via its given Rexp 
    (the child it can traverse downwards from)
 
-}
der ::  Char -> Zipper -> [Zipper]
der c (Zipper re ctx) = up re ctx
    where
    down :: Context -> Rexp -> [Zipper]
    down ct (CHAR d)
        | c == d = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct (RANGE cs)
        | Set.member c cs = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct r@(SEQ _ []) = up r ct
    down ct (SEQ s (r:rs)) =
        if nullable r then
            case rs of
                [] -> down (SeqC ct s [] rs) r
                (r':rs') ->
                    let z1 = down (SeqC ct s [] rs) r
                        z2 = down (SeqC ct s [] rs') r'
                    in z1 ++ z2
        else down (SeqC ct s [] rs) r
    down ct (ALT rs) = concatMap (down (AltC ct)) rs
    down ct (STAR r) = down (StarC ct [] r) r
    down ct (PLUS r) = down (StarC ct [] r) r
    down ct (OPTIONAL r) = down (OptionalC ct) r
    down ct r@(NTIMES 0 _) = up r ct
    down ct (NTIMES n r) =
        let r' = NTIMES (n-1) r
            ctt = SeqC ct sBottom [] [r']
            zs = down ctt r
        in 
            if nullable r then
                let zs' = up r' ct
                in (zs ++ zs')
            else zs
    down ct (RECD s r') = down (RecdC ct s) r'

    up :: Rexp -> Context -> [Zipper]
    up _ TopC = []
    up r (SeqC ct s rs []) = up (SEQ s (reverse (r:rs))) ct
    up r (SeqC ct s left (r':right)) =
        let zs = down (SeqC ct s (r:left) right) r'
        in if nullable r' && null zs then
            up r (SeqC ct s left right)
        else zs
    up r (AltC ct) = up (ALT [r]) ct
    up r (StarC ct rs r') =
        let zs = down (StarC ct (r:rs) r') r' in
            if null zs then
                up (defaultSEQ (reverse (r:rs))) ct
            else zs
    up r (OptionalC ct) = up r ct
    up r (NTimesC ct 0 rs _) = up (defaultSEQ (reverse (r:rs))) ct
    up r (RecdC ct s) = up (RECD s r) ct

{-
    Derives an Rexp from a String (List of characters)
    It does so by taking successive derivatives on the Rexp
    wrt each front character of the string, until there are
    no more characters in the string to process.

    It then filters all of the zippers that are nullable.
-}
ders :: [Char] -> [Zipper] -> [Zipper]
ders [] zs = concatMap (der cBottom) zs
ders (c:cs) zs = ders cs (concatMap (der c) zs)

getNullableZippers :: [Zipper] -> [Zipper]
getNullableZippers = concatMap (\z@(Zipper r' ct) -> ([z | nullable r' && isNullable ct]))

matcher :: [Char] -> Rexp -> Bool
matcher [] r = nullable r
matcher s r =
    let zs = ders s [focus r] in
        not (null zs)

{- 
    Whether the parent tree is valid - the resultant 
    derivative taken can match the empty string.
    
    The equivalent of going all the way up the zipper,
    without recording the matched values.
    
    Used for matching
-}
isNullable :: Context -> Bool
isNullable TopC = False
isNullable (NTimesC ct n _ _) = n == 0 && isNullable ct
isNullable (AltC ct) = isNullable ct
isNullable (SeqC TopC _ _ _) = True
isNullable (SeqC ct _ _ es) = all nullable es && isNullable ct
isNullable (StarC ct _ _) = isNullable ct
isNullable (RecdC ct _) = isNullable ct

{-
    Returns the lexed result from taking successive 
    derivatives on the Rexp wrt the string.
-}
lexing :: [Char] -> Rexp -> [Rexp]
lexing cs r = map getRexpFromZipper (ders cs [focus r])

lexSimp :: [Char] -> Rexp -> [[([Char], [Char])]]
lexSimp s r = map env (lexing s r)

getRexpFromZipper :: Zipper -> Rexp
getRexpFromZipper (Zipper _ (SeqC TopC _ rs _)) = head rs
getRexpFromZipper _ = error "not valid structure"

plug :: Rexp -> Context -> Zipper
plug r (SeqC TopC _ _ _) = Zipper r TopC
plug r (SeqC ct s rs _) = plug (SEQ s (reverse (r:rs))) ct
plug r (AltC ct) = plug r ct
plug r (StarC ct rs _) = plug (STAR (defaultSEQ (reverse (r:rs)))) ct
plug r (NTimesC ct 0 es _) = plug (NTIMES 0 (defaultSEQ (reverse (r:es)))) ct
plug r (RecdC ct s) = plug (RECD s r) ct
plug _ _ = error "Could not find a path from the leaf to the root"

flatten :: Rexp -> [Char]
flatten (SEQ c [])
    | c == sBottom = []
    | otherwise = [c]
flatten (SEQ s rs)
    | s == sBottom = concatMap flatten rs
    | otherwise = s:concatMap flatten rs
flatten (ALT rs) = concatMap flatten rs
flatten (RECD _ r) = flatten r

env :: Rexp -> [([Char], [Char])]
env (ALT rs) = concatMap env rs
env (SEQ _ rs) = concatMap env rs
env (STAR r) = env r
env (NTIMES _ r) = env r
env (RECD s r) = (s, flatten r) : env r

{- 
    Converting a string to a regular expression 
    without explicitly using the constructors. 
-}
stringToRexp :: [Char] -> Rexp
stringToRexp [] = defaultSEQ []
stringToRexp [c] = CHAR c
stringToRexp (c:cs) = defaultSEQ (CHAR c:map CHAR cs)

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
a <~> b =
    case (toRexp a, toRexp b) of
        (SEQ _ xs, SEQ _ ys) -> defaultSEQ (xs ++ ys)
        (ae, be) -> defaultSEQ [ae,be]

(<|>) :: (ToRexp a, ToRexp b) => a -> b -> Rexp
a <|> b =
    case (toRexp a, toRexp b) of
    (ALT xs, ALT ys) -> ALT (xs ++ ys)
    (ALT xs, be) -> ALT (xs ++ [be])
    (ae, ALT ys) -> ALT (ae:ys)
    (ae, be) -> ALT [ae, be]

(<$>) :: String -> Rexp -> Rexp
s <$> r = RECD s r

(*>) :: ToRexp a => a -> b -> Rexp
r *> _ = STAR (toRexp r)

(+>) :: ToRexp a => a -> b -> Rexp
r +> _ = PLUS (toRexp r)

(?>) :: ToRexp a => a -> b -> Rexp
r ?> _ = OPTIONAL (toRexp r)

(^>) :: ToRexp a => a -> Int -> Rexp
r ^> n = NTIMES n (toRexp r)


-- pretty-printing REGs
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
pp (CHAR c)     = c : "\n"
pp (RANGE cs)     = Set.showTreeWith True False cs ++ "\n"
pp (SEQ s rs)  =  (if null rs then [s] else "SEQ\n" ++ pps rs) ++ "\n"
pp (ALT rs) = "ALT\n" ++ pps rs
pp (STAR r)    = "STAR\n" ++ pp r
pp (RECD s r)    = "RECD\n" ++ s ++ "\n" ++ pp r
pp (NTIMES n r)    = "NTIMES\n" ++ show n ++ " " ++ pp r ++ "\n"

ppz :: Zipper -> String
ppz (Zipper r ct) = "ZIP\n" ++ indent (pp r:[ppctx ct])

ppctx :: Context -> String
ppctx TopC = "Top\n"
ppctx (SeqC ct _ _ _) = "SEQC\n" ++ indent [ppctx ct]
ppctx (AltC ct) = "ALTC\n" ++ indent [ppctx ct]
ppctx (StarC ct _ _) = "STARC\n" ++ indent [ppctx ct]
ppctx (RecdC ct _) = "RECDC\n" ++ indent [ppctx ct]
ppctx (NTimesC ct _ _ _) = "NTIMESC\n" ++ indent [ppctx ct]

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RexpZipper where
import Test.QuickCheck
import Token ( Token, token )
import qualified Data.Set as Set
import Data.List (intercalate)


type Sym = Char

data Exp = ZERO
            | ONE
            | CHAR Char
            | RANGE (Set.Set Char)
            | SEQ Sym [Exp]
            | ALT [Exp]
            | STAR Exp
            | NTIMES Int Exp -- number of repetitions left, the Exp it represents, the processed Exps, whether it is nullable or not - a little expensive tho? you're still going down the whole tree once
            | RECD [Char] Exp
            deriving (Ord, Eq, Show)

{-
    Default constructors for Exps that remember how 
    they were matched.

    They all start off with having an empty list.
-}

defaultSEQ :: [Exp] -> Exp
defaultSEQ = SEQ '\0'

defaultSTAR :: Exp -> Exp
defaultSTAR = STAR

defaultPLUS :: Exp -> Exp
defaultPLUS r = defaultSEQ [r, defaultSTAR r]

defaultOPTIONAL :: Exp -> Exp
defaultOPTIONAL r = ALT [ONE, r]

defaultNTIMES :: Int -> Exp -> Exp
defaultNTIMES = NTIMES

defaultRECD :: String -> Exp -> Exp
defaultRECD = RECD

data Context = TopC
            | SeqC Context Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Context -- Alternate has one context shared between its children
            | StarC Context [Exp] Exp
            | NTimesC Context Int [Exp] Exp
            | RecdC Context [Char]
            deriving (Ord, Eq, Show)

data Zipper = Zipper Exp Context deriving (Ord, Eq, Show)

instance Arbitrary Exp where
    arbitrary = oneof
        [ return ZERO
        , return ONE
        , CHAR Prelude.<$> arbitrary
        , RANGE Prelude.<$> arbitrary
        , SEQ Prelude.<$> arbitrary <*> listOf arbitrary
        , ALT Prelude.<$> listOf arbitrary
        , STAR Prelude.<$> arbitrary
        ]

{- 
    Whether an Exp is nullable (can match the empty string).
-}
nullable :: Exp -> Bool
nullable ZERO = False
nullable ONE = True
nullable (CHAR _) = False
nullable (RANGE _) = False
nullable (ALT es) = any nullable es
nullable (SEQ _ es) = all nullable es
nullable (STAR _) = True
nullable (NTIMES 0 _) = True
nullable (NTIMES _ r) = nullable r
nullable (RECD _ r) = nullable r

{-
    Creates a zipper that focuses on 
    the expression.
-}

focus :: Exp -> Zipper
focus r = Zipper r (SeqC TopC '\0' [] [r])

{-
    Derives the result of an Exp from one character:
    The semantic derivative on a language L wrt a character c
    is as follows: Der L c = { s | c:s is in L }

    Hence the result of taking the successful derivative on 
    a zipper is a zipper that focuses on c and remembers 
    what was around it. 

    It has two functions:
    up - moves back up the zipper via its given Context 
    (the parent it can traverse upwards from)
    down - moves down the zipper via its given Exp 
    (the child it can traverse downwards from)
 
-}
der ::  Char -> Zipper -> [Zipper]
der c (Zipper re ctx) = up re ctx
    where
    down :: Context -> Exp -> [Zipper]
    down _ ZERO = []
    down _ ONE = []
    down ct (CHAR d)
        | c == d = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct (RANGE cs)
        | Set.member c cs = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct r@(SEQ _ []) = up r ct
    down ct (SEQ s (e:es)) =
        if nullable e then
            case es of
                [] -> down (SeqC ct s [] es) e
                (er:esr) ->
                    let z1 = down (SeqC ct s [] es) e
                        z2 = down (SeqC ct s [] esr) er
                    in z1 ++ z2
        else down (SeqC ct s [] es) e
    down ct (ALT es) = concatMap (down (AltC ct)) es
    down ct (STAR e) = down (StarC ct [] e) e  
    down ct r@(NTIMES 0 _) = up r ct
    down ct (NTIMES n e) =
        let e' = NTIMES (n-1) e
            ctt = SeqC ct '\0' [] [e']
        in down ctt e
    down ct (RECD s r') = down (RecdC ct s) r'

    up :: Exp -> Context -> [Zipper]
    up _ TopC = []
    up e (SeqC ct s es []) = up (SEQ s (reverse (e:es))) ct
    up e (SeqC ct s el (er:esr)) = 
        let zs = down (SeqC ct s (e:el) esr) er
        in if nullable er then
            case esr of
                (err:esrs) -> 
                    let zs' = down (SeqC ct s (e:el) esrs) err
                    in (zs ++ zs')
                [] -> 
                    if null zs then 
                        up e (SeqC ct s el esr)
                    else zs
        else zs
    up e (AltC ct) = up (ALT [e]) ct
    up e (StarC ct es r) =
        let zs = down (StarC ct (e:es) r) r in
            if null zs then
                up (STAR (defaultSEQ (reverse (e:es)))) ct
            else zs
    up e (NTimesC ct 0 es _) = up (NTIMES 0 (defaultSEQ (reverse (e:es)))) ct
    up e (NTimesC ctt n es r) =
        if nullable e then
            down (NTimesC ctt (n-1) (e:es) r) r
            ++ down (NTimesC ctt (n-1) es r) r
        else
            down (NTimesC ctt (n-1) (e:es) r) r
    up e (RecdC ct s) = up (RECD s e) ct

{-
    Derives an Exp from a String (List of characters)
    It does so by taking successive derivatives on the Exp
    wrt each front character of the string, until there are
    no more characters in the string to process.

    It then filters all of the zippers that are nullable.
-}
ders :: [Char] -> [Zipper] -> [Zipper]
ders [] zs = zs
ders (c:cs) zs = ders cs (concatMap (der c) zs)

getNullableZippers :: [Zipper] -> [Zipper]
getNullableZippers = concatMap (\z@(Zipper r' ct) -> ([z | nullable r' && isNullable ct]))

matcher :: [Char] -> Exp -> Bool
matcher [] r = nullable r
matcher s r =
    let zs = getNullableZippers (ders s [focus (simp r)]) in
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
    derivatives on the Exp wrt the string.

    TODO: fix this
-}
lexing :: [Char] -> Exp -> [Exp]
lexing cs e =
    let zs = getNullableZippers (ders cs [focus (simp e)]) in
        map (\(Zipper r' ct) -> getExpFromZipper (plug r' ct)) zs

lexSimp :: [Char] -> Exp -> [[([Char], [Char])]]
lexSimp s r = map env (lexing s r)

getExpFromZipper :: Zipper -> Exp
getExpFromZipper (Zipper r _) = r

plug :: Exp -> Context -> Zipper
plug e (SeqC TopC _ _ _) = Zipper e TopC
plug e (SeqC ct s es _) = plug (SEQ s (reverse (e:es))) ct
plug e (AltC ct) = plug e ct
plug e (StarC ct es _) = plug (STAR (defaultSEQ (reverse (e:es)))) ct
plug e (NTimesC ct 0 es _) = plug (NTIMES 0 (defaultSEQ (reverse (e:es)))) ct
plug e (RecdC ct s) = plug (RECD s e) ct
plug _ _ = error "Could not find a path from the leaf to the root"

flatten :: Exp -> [Char]
flatten ZERO = error "Cannot flatten ZERO"
flatten ONE = []
flatten (CHAR _) = []
flatten (RANGE _) = []
flatten (SEQ c [])
    | c == '\0' = []
    | otherwise = [c]
flatten (SEQ s es)
    | s == '\0' = concatMap flatten es
    | otherwise = s:concatMap flatten es
flatten (ALT es) = concatMap flatten es
flatten (STAR e) = flatten e
flatten (NTIMES _ e) = flatten e
flatten (RECD _ e) = flatten e

env :: Exp -> [([Char], [Char])]
env ZERO = error "ZERO is an invalid input for `env`"
env ONE = []
env (CHAR _) = []
env (RANGE _) = []
env (ALT es) = concatMap env es
env (SEQ _ es) = concatMap env es
env (STAR e) = env e
env (NTIMES _ e) = env e
env (RECD s e) = (s, flatten e) : env e

simp :: Exp -> Exp
simp (SEQ s es) = SEQ s (map simp (filter (/= ONE) es))
simp (ALT es) = ALT (map simp (filter (/= ZERO) es))
simp (STAR (STAR e)) = simp (STAR e)
simp r = r

{- 
    Converting a string to a regular expression 
    without explicitly using the constructors. 
-}
stringToExp :: [Char] -> Exp
stringToExp [] = ONE
stringToExp [c] = CHAR c
stringToExp (c:cs) = defaultSEQ (CHAR c:map CHAR cs)

class ToExp a where
  toExp :: a -> Exp

instance ToExp Exp where
  toExp :: Exp -> Exp
  toExp = id

instance ToExp String where
  toExp = stringToExp

infixl 9 ^>
infixl 8 ?>
infixl 7 +>
infixl 6 *>
infixl 4 <~>
infixl 3 <|>
infixl 1 <$>

(<~>) :: (ToExp a, ToExp b) => a -> b -> Exp
a <~> b = 
    case (toExp a, toExp b) of
        (SEQ _ xs, SEQ _ ys) -> defaultSEQ (xs ++ ys)
        (ae, be) -> defaultSEQ [ae,be]

(<|>) :: (ToExp a, ToExp b) => a -> b -> Exp
a <|> b =
    case (toExp a, toExp b) of
    (ALT xs, ALT ys) -> ALT (xs ++ ys)
    (ALT xs, be) -> ALT (xs ++ [be])
    (ae, ALT ys) -> ALT (ae:ys)
    (ae, be) -> ALT [ae, be]

(<$>) :: String -> Exp -> Exp
s <$> r = defaultRECD s r

(*>) :: ToExp a => a -> b -> Exp
r *> _ = defaultSTAR (toExp r)

(+>) :: ToExp a => a -> b -> Exp
r +> _ = defaultPLUS (toExp r)

(?>) :: ToExp a => a -> b -> Exp
r ?> _ = defaultOPTIONAL (toExp r)

(^>) :: ToExp a => a -> Int -> Exp
r ^> n = defaultNTIMES n (toExp r)

{- WHILE Language registers - NOTE: doesn't work rn -}
keyword :: Exp
keyword = "while" <|> "if" <|> "then" <|> "else" <|> "do" <|> "for" <|>
          "to" <|> "true" <|> "false" <|> "read" <|> "write" <|>
          "skip" <|> "break"

op :: Exp
op = ">" <|> "<" <|> "==" <|> "!=" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||" <|> "+" <|> "-" <|> "*" <|> "%" <|> "/"

lett :: Exp
lett = RANGE $ Set.fromList (['A'..'Z'] ++ ['a'..'z'])

sym :: Exp
sym = lett <|> RANGE (Set.fromList ['.', '_', '>', '<', '=', ';', ',', '\\', ':'])

parens :: Exp
parens = RANGE $ Set.fromList ['(', ')', '{', '}']

digit :: Exp
digit = RANGE $ Set.fromList ['0'..'9']

semi :: Exp
semi = toExp ";"

whitespace :: Exp
whitespace = (" " <|> "\n" <|> "\t" <|> "\r") RexpZipper.+> ()

identifier :: Exp
identifier = lett <~> (("_" <|> lett <|> digit) RexpZipper.*> ())

numbers :: Exp
numbers = "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> (digit RexpZipper.*> ()))

string :: Exp
string = "\"" <~> ((sym <|> digit <|> parens <|> whitespace <|> "\n") RexpZipper.*> ()) <~> "\""

eol :: Exp
eol = "\n" <|> "\r\n"

comment :: Exp
comment = "//" <~> ((sym <|> parens <|> digit <|> toExp " " RexpZipper.*> ()) RexpZipper.*> ()) <~> eol

whileRegs :: Exp
whileRegs = (("k" RexpZipper.<$> keyword)
            <|> ("c" RexpZipper.<$> comment)    
            <|> ("o" RexpZipper.<$> op)
            <|> ("str" RexpZipper.<$> string)
            <|> ("p" RexpZipper.<$> parens)
            <|> ("s" RexpZipper.<$> semi)
            <|> ("w" RexpZipper.<$> whitespace)
            <|> ("i" RexpZipper.<$> identifier)
            <|> ("n" RexpZipper.<$> numbers)) RexpZipper.*> ()

-- whileRegs :: Exp
-- whileRegs = (("k" Z.<$> Z.keyword)
--             Z.<|> ("o" Z.<$> Z.op)
--             Z.<|> ("str" Z.<$> Z.string)
--             Z.<|> ("p" Z.<$> Z.parens)
--             Z.<|> ("s" Z.<$> Z.semi)
--             Z.<|> ("w" Z.<$> Z.whitespace)
--             Z.<|> ("i" Z.<$> Z.identifier)
--             Z.<|> ("n" Z.<$> Z.numbers)
--             Z.<|> ("c" Z.<$> Z.comment)) Z.*> ()

tokenise :: String -> [Token]
tokenise s = map token $ filter isNotWhitespace $ head $ lexSimp s whileRegs
  where isNotWhitespace ("w", _) = False
        isNotWhitespace ("c", _) = False
        isNotWhitespace _ = True


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

pps :: [Exp] -> String
pps es = indent (map pp es)

pp :: Exp -> String
pp ZERO        = "0\n"
pp ONE         = "1\n"
pp (CHAR c)     = c : "\n"
pp (RANGE cs)     = Set.showTreeWith True False cs ++ "\n"
pp (SEQ s es)  =  (if null es then [s] else "SEQ\n" ++ pps es) ++ "\n"
pp (ALT es) = "ALT\n" ++ pps es
pp (STAR e)    = "STAR\n" ++ pp e
pp (RECD s e)    = "RECD\n" ++ s ++ "\n" ++ pp e
pp (NTIMES n e)    = "NTIMES\n" ++ show n ++ " " ++ pp e ++ "\n"

ppz :: Zipper -> String
ppz (Zipper r ct) = "ZIP\n" ++ indent (pp r:[ppctx ct])

ppctx :: Context -> String
ppctx TopC = "Top\n"
ppctx (SeqC ct _ _ _) = "SEQC\n" ++ indent [ppctx ct]
ppctx (AltC ct) = "ALTC\n" ++ indent [ppctx ct]
ppctx (StarC ct _ _) = "STARC\n" ++ indent [ppctx ct]
ppctx (RecdC ct _) = "RECDC\n" ++ indent [ppctx ct]
ppctx (NTimesC ct _ _ _) = "NTIMESC\n" ++ indent [ppctx ct]

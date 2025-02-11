{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RexpZipper where
import Test.QuickCheck
import Token ( Token, token )
import qualified Data.Set as Set

type Sym = Char

data Exp = ZERO
            | ONE
            | CHAR Char
            | RANGE (Set.Set Char)
            | SEQ Sym [Exp]
            | ALT [Exp]
            | STAR Exp [Exp]
            -- | PLUS Exp [Exp]
            -- | OPTIONAL Exp [Exp]  -- equivalent to 1 + STAR r - but i want to make it its own constructor to reduce the number of nodes in the tree
            | NTIMES Int Exp [Exp] Bool -- number of repetitions left, the Exp it represents, the processed Exps, whether it is nullable or not - a little expensive tho? you're still going down the whole tree once
            | RECD [Char] Exp [Exp] deriving (Ord, Eq, Show)

{-
    Default constructors for Exps that remember how 
    they were matched.

    They all start off with having an empty list.
-}

defaultSEQ :: [Exp] -> Exp
defaultSEQ = SEQ '\0'

defaultSTAR :: Exp -> Exp
defaultSTAR r = STAR r []

defaultPLUS :: Exp -> Exp
defaultPLUS r = defaultSEQ [r, defaultSTAR r]

defaultOPTIONAL :: Exp -> Exp
defaultOPTIONAL r = ALT [ONE, r]

defaultNTIMES :: Int -> Exp -> Exp
defaultNTIMES n r = NTIMES n r [] (nullable r)

defaultRECD :: String -> Exp -> Exp
defaultRECD s e = RECD s e []

data Context = TopC
            | SeqC Context Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Context -- Alternate has one context shared between its children
            | StarC Context [Exp] Exp
            -- | PlusC Context [Exp] Exp
            -- | OptionalC Context [Exp] Exp
            | NTimesC Context Int [Exp] Exp Bool
            | RecdC Context Exp [Char] [Exp] deriving (Ord, Eq, Show)

data Zipper = Zipper Exp Context deriving (Ord, Eq, Show)

instance Arbitrary Exp where
    arbitrary = oneof
        [ return ZERO
        , return ONE
        , CHAR Prelude.<$> arbitrary
        , RANGE Prelude.<$> arbitrary
        , SEQ Prelude.<$> arbitrary <*> listOf arbitrary
        , ALT Prelude.<$> listOf arbitrary
        , STAR Prelude.<$> arbitrary <*> listOf arbitrary
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
nullable (STAR _ _) = True
-- nullable (PLUS e _) = nullable e
-- nullable (OPTIONAL _ _) = True
nullable (NTIMES 0 _ _ _) = True
nullable (NTIMES _ r _ _) = nullable r
nullable (RECD _ r _) = nullable r

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
    down ct (RANGE cs)
        | Set.member c cs = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct r@(SEQ _ []) = up r ct
    down ct (SEQ s (e:es)) =
        if nullable e then
        down (SeqC ct s [] es) e ++ up (SEQ s es) (SeqC ct s [] es)
        else down (SeqC ct s [] es) e
    down ct (ALT es) = concatMap (down (AltC ct)) es
    down ct r@(STAR e es) =
        let zs = down (StarC ct es e) e in
            if null zs then
                 up r ct
            else zs
    -- down ct (OPTIONAL e es) =
    --     if not (null es) then [] else
    --     let zs = down (OptionalC ct es e) e in
    --         if null zs then
    --             up ONE (OptionalC ct es e)
    --         else zs
    -- down ct r@(PLUS e es) =
    --     let zs = down (PlusC ct es e) e in
    --         if null zs then
    --             if null es then [] else up r ct
    --         else zs
    down ct r@(NTIMES 0 _ _ _) = up r ct
    down ct r@(NTIMES n e es nu) =
        let ctt = NTimesC ct (n-1) es e nu
            zs = down ctt e in
            if nu then zs ++ up ONE ctt else
            if null zs then
                up r ct
            else zs
    down ct (RECD s r' es) = down (RecdC ct r' s es) r'

    up :: Exp -> Context -> [Zipper]
    up _ TopC = []
    up e (SeqC ct s es []) = up (SEQ s (reverse (e:es))) ct
    up e (SeqC ct s el (er:esr)) = down (SeqC ct s (e:el) esr) er
    up e (AltC ct) = up (ALT [e]) ct
    up e (StarC ct es r) =
        let zs = down (StarC ct (e:es) r) r in
            if null zs then
                up (STAR r (reverse (e:es))) ct
            else zs
    -- up e (OptionalC ct es r) = up (OPTIONAL r (reverse(e:es))) ct
    -- up e (PlusC ct es r) =
    --     let zs = down (PlusC ct (e:es) r) r in
    --         if null zs then
    --             up (PLUS r (reverse (e:es))) ct
    --         else zs
    up e (NTimesC ct 0 es r _) = up (NTIMES 0 r (reverse (e:es)) True) ct
    up e (NTimesC ctt n es r nu) = down (NTimesC ctt (n-1) (e:es) r nu) r
    up e (RecdC ct r s es) = down (RecdC ct r s (e:es)) e

{-
    Derives an Exp from a String (List of characters)
    It does so by taking successive derivatives on the Exp
    wrt each front character of the string, until there are
    no more characters in the string to process.

    It then filters all of the zippers that are nullable.
-}
ders :: [Char] -> [Zipper] -> [Zipper]
ders [] zs = concatMap (\z@(Zipper r _)-> ([z | nullable r])) zs
ders (c:cs) zs = ders cs (concatMap (der c) zs)

matcher :: [Char] -> Exp -> Bool
matcher [] r = nullable r
matcher s r =
    let zs = ders s [focus r] in
        not (null zs) && any (\(Zipper _ ct) -> isNullable ct) zs

{- 
    Whether the parent tree is valid - the resultant 
    derivative taken can match the empty string.
    
    The equivalent of going all the way up the zipper,
    without recording the matched values.
    
    Used for matching
-}
isNullable :: Context -> Bool
isNullable TopC = False
isNullable (NTimesC ct n _ _ _) = n == 0 && isNullable ct
isNullable (AltC ct) = isNullable ct
isNullable (SeqC TopC _ _ _) = True
isNullable (SeqC ct _ _ es) = null es && isNullable ct
isNullable (StarC ct _ _) = isNullable ct
-- isNullable (OptionalC ct _ _) = isNullable ct
-- isNullable (PlusC ct es _) = not (null es) && isNullable ct
isNullable (RecdC ct _ _ _) = isNullable ct

{-
    Returns the lexed result from taking successive 
    derivatives on the Exp wrt the string.

    TODO: fix this
-}
lexing :: [Char] -> Exp -> Exp
lexing cs e =
    let Zipper re ctx = head (ders cs [focus e]) in
        if isNullable ctx && nullable re then
            let (Zipper r' _) = plug re ctx in
                r'
        else error "Could not lex"

lexSimp :: [Char] -> Exp -> [([Char], [Char])]
lexSimp s r = env $ lexing s r

plug :: Exp -> Context -> Zipper
plug e (SeqC TopC _ _ _) = Zipper e TopC
plug e (SeqC ct s es _) = plug (SEQ s (reverse(e:es))) ct
plug e (AltC ct) = plug e ct
plug e (StarC ct es r) = plug (STAR r (reverse (e:es))) ct
plug e (NTimesC ct 0 es r _) = plug (NTIMES 0 r (reverse (e:es)) True) ct
plug e (RecdC ct r s es) = plug (RECD s r (reverse (e:es))) ct
plug _ _ = error "Could not find a path from the leaf to the root"

flatten :: Exp -> [Char]
flatten ZERO = error "Cannot flatten ZERO"
flatten ONE = []
flatten (CHAR _) = []
flatten (RANGE _) = []
flatten (RANGE _) = []
flatten (SEQ c [])
    | c == '\0' = []
    | otherwise = [c]
flatten (SEQ s es)
    | s == '\0' = concatMap flatten es
    | otherwise = s:concatMap flatten es
flatten (ALT es) = concatMap flatten es
flatten (STAR _ es) = concatMap flatten es
-- flatten (PLUS _ es) = concatMap flatten es
flatten (NTIMES _ _ es _) = concatMap flatten es
flatten (RECD _ _ es) = concatMap flatten es

env :: Exp -> [([Char], [Char])]
env ZERO = error "ZERO is an invalid input for `env`"
env ONE = []
env (CHAR _) = []
env (RANGE _) = []
env (RANGE _) = []
env (ALT es) = concatMap env es
env (SEQ _ es) = concatMap env es
env (STAR _ es) = concatMap env es
-- env (PLUS _ es) = concatMap env es
env (NTIMES _ _ es _) = concatMap env es
env (RECD s _ es) = (s, concatMap flatten es) : concatMap env es

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

-- TODO: fix this
(<~>) :: (ToExp a, ToExp b) => a -> b -> Exp
a <~> b = defaultSEQ [toExp a, toExp b]
    -- case (toExp a, toExp b) of
    --     (SEQ _ xs, SEQ _ ys) -> defaultSEQ (xs ++ ys)
    --     (ae, be) -> defaultSEQ [ae,be]

-- TODO: fix this
(<|>) :: (ToExp a, ToExp b) => a -> b -> Exp
a <|> b = ALT [toExp a, toExp b]
    -- case (toExp a, toExp b) of
    --     (ALT xs, ALT ys) -> ALT (xs ++ ys)
    --     (ALT xs, be) -> ALT (be:xs)
    --     (ae, ALT ys) -> ALT (ae:ys)
    --     (ae, be) -> ALT [ae, be]

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
op = "+" <|> "-" <|> "*" <|> "%" <|> "/" <|> "==" <|> "!=" <|> ">" <|> 
     "<" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||"

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
whitespace = (" " <|> "\n" <|> "\t" <|> "\r") +> ()

identifier :: Exp
identifier = lett <~> (("_" <|> lett <|> digit) RexpZipper.*> ())

numbers :: Exp
numbers = "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> (digit RexpZipper.*> ()))

string :: Exp
string = "\"" <~> ((sym <|> digit <|> parens <|> whitespace <|> "\n") RexpZipper.*> ()) <~> "\""

eol :: Exp
eol = "\n" <|> "\r\n"

comment :: Exp
comment = "//" <~> ((sym <|> parens <|> digit <|> toExp " ") RexpZipper.*> ()) <~> eol

whileRegs :: Exp
whileRegs = (("k" RexpZipper.<$> keyword)
            <|> ("o" RexpZipper.<$> op)
            <|> ("str" RexpZipper.<$> string)
            <|> ("p" RexpZipper.<$> parens)
            <|> ("s" RexpZipper.<$> semi)
            <|> ("w" RexpZipper.<$> whitespace)
            <|> ("i" RexpZipper.<$> identifier)
            <|> ("n" RexpZipper.<$> numbers)
            <|> ("c" RexpZipper.<$> comment)) RexpZipper.*> ()

tokenise :: String -> [Token]
tokenise s = map token $ filter isNotWhitespace $ lexZipper s whileRegs 
  where isNotWhitespace ("w", _) = False
        isNotWhitespace ("c", _) = False
        isNotWhitespace _ = True
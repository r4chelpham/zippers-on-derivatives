{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RexpZipper where
import Test.QuickCheck


type Sym = Char

data Exp = ZERO
            | ONE
            | CHAR Char
            | SEQ Sym [Exp]
            | ALT [Exp]
            | STAR Exp [Exp]
            | PLUS Exp [Exp]
            -- | OPTIONAL Exp [Exp]  -- equivalent to 1 + STAR r - but i want to make it its own constructor to reduce the number of nodes in the tree
            | NTIMES Int Exp [Exp] Bool -- number of repetitions left, the Exp it represents, the processed Exps, whether it is nullable or not - a little expensive tho? you're still going down the whole tree once
            | RECD [Char] Exp [Exp] deriving (Ord, Eq, Show)

defaultSEQ :: [Exp] -> Exp
defaultSEQ = SEQ '\0'

defaultSTAR :: Exp -> Exp
defaultSTAR r = STAR r []

defaultPLUS :: Exp -> Exp
defaultPLUS r = PLUS r []

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
            | PlusC Context [Exp] Exp
            -- | OptionalC Context [Exp] Exp
            | NTimesC Context Int [Exp] Exp Bool
            | RecdC Context Exp [Char] [Exp] deriving (Ord, Eq, Show)

data Zipper = Zipper Exp Context deriving (Ord, Eq, Show)

instance Arbitrary Exp where
    arbitrary = oneof
        [ return ZERO
        , return ONE
        , CHAR Prelude.<$> arbitrary
        , SEQ Prelude.<$> arbitrary <*> listOf arbitrary
        , ALT Prelude.<$> listOf arbitrary
        , STAR Prelude.<$> arbitrary <*> listOf arbitrary
        ]

nullable :: Exp -> Bool
nullable ZERO = False
nullable ONE = True
nullable (CHAR _) = False
nullable (ALT es) = any nullable es
nullable (SEQ _ es) = all nullable es
nullable (STAR _ _) = True
nullable (PLUS e _) = nullable e
-- nullable OPTIONAL _ = True
nullable (NTIMES 0 _ _ _) = True
nullable (NTIMES _ r _ _) = nullable r
nullable (RECD _ r _) = nullable r

focus :: Exp -> Zipper
focus r = Zipper r (SeqC TopC '\0' [] [r])

der ::  Char -> Zipper -> [Zipper]
der c (Zipper re ctx) = up re ctx
    where
    down :: Context -> Exp -> [Zipper]
    down _ ZERO = []
    down ct ONE = up (defaultSEQ [ONE]) ct
    down ct (CHAR d)
        | c == d = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct r@(SEQ _ []) = up r ct
    down ct (SEQ s (e:es)) = down (SeqC ct s [] es) e
    down ct (ALT es) = concatMap (down (AltC ct)) es
    down ct r@(STAR e es)
        | c == '\0' = up r ct
        | otherwise = 
            let zs = down (StarC ct es e) e in
                if null zs then up (defaultSEQ [ONE]) ct
                else zs
    down ct r@(PLUS e es)
        | c == '\0' = if null es then [] else up r ct
        | otherwise = 
            let zs = down (PlusC ct es e) e in 
                if null zs then
                    if null es then [] else up r ct
                else zs
    down ct r@(NTIMES 0 _ _ _) = up r ct
    down ct r@(NTIMES n e es nu)
        | c == '\0' && not nu = []
        | otherwise =
            let zs = down (NTimesC ct (n-1) es e nu) e in
                if null zs then
                    if nu then up (defaultSEQ [ONE]) ct else up r ct
                else zs
    down ct (RECD s r' es) = down (RecdC ct r' s es) r'
    up :: Exp -> Context -> [Zipper]
    up _ TopC = []
    up e (SeqC ct s es [])
        | c == '\0' && ct == TopC =
            -- zipper is at the top of the tree
            [Zipper (SEQ s [e]) ct]
        | otherwise = up (SEQ s (reverse (e:es))) ct
    up e (SeqC ct s el (er:esr)) = down (SeqC ct s (e:el) esr) er
    up e (AltC ct) = up (ALT [e]) ct
    up e (StarC ct es r)
        | c == '\0' = up (STAR r (reverse (e:es))) ct
        | otherwise =
            let zs = down (StarC ct (e:es) r) r in
                    if null zs then
                        up (STAR r (reverse (e:es))) ct
                    else zs
    up e (PlusC ct es r)
        | c == '\0' = up (PLUS r (reverse (e:es))) ct
        | otherwise = 
            let zs = down (PlusC ct (e:es) r) r in 
                if null zs then
                    up (PLUS r (reverse (e:es))) ct
                else zs
    up e (NTimesC ct 0 es r _) = up (NTIMES 0 r (reverse (e:es)) True) ct
    up e (NTimesC ctt n es r nu)
        | c == '\0' =
            if n /= 0 && not nu then [] else
            let exps = reverse (e:es) in
            case ctt of
                TopC -> [Zipper (NTIMES (n - 1) r exps nu) ctt]
                _ -> up (NTIMES (n - 1) r exps nu) ctt
        | otherwise =
            let exps = (e:es) in
            down (NTimesC ctt (n-1) exps r nu) r
    up e (RecdC ct r s es)
        | c == '\0' = up (RECD s r [e]) ct
        | otherwise = down (RecdC ct r s (e:es)) e

ders :: [Char] -> [Zipper] -> [Zipper]
ders cs zs =
    let res = foldl (\ z c -> concatMap (der c) z) zs cs in
        if null res then [] else der '\0' (head res)

matcher :: [Char] -> Exp -> Bool
matcher s r = not $ null $ ders s [focus r]

lex :: [Char] -> Exp -> [Char]
lex cs e =
    let (Zipper re _) = head (ders cs [focus e]) in
        flatten re

flatten :: Exp -> [Char]
flatten ZERO = error "Cannot flatten ZERO"
flatten ONE = []
flatten (CHAR _) = []
flatten (SEQ c [])
    | c == '\0' = []
    | otherwise = [c]
flatten (SEQ s es)
    | s == '\0' = concatMap flatten es
    | otherwise = s:concatMap flatten es
flatten (ALT es) = concatMap flatten es
flatten (STAR _ es) = concatMap flatten es
flatten (PLUS _ es) = concatMap flatten es
flatten (NTIMES _ _ es _) = concatMap flatten es
flatten (RECD _ _ es) = concatMap flatten es

env :: Exp -> [([Char], [Char])]
env ZERO = error "ZERO is an invalid input for `env`"
env ONE = []
env (CHAR _) = []
env (ALT es) = concatMap env es
env (SEQ _ es) = concatMap env es
env (STAR _ es) = concatMap env es
env (PLUS _ es) = concatMap env es
env (NTIMES _ _ es _) = concatMap env es
env (RECD s _ es) = (s, concatMap flatten es) : concatMap env es

{- Converting a string to a regular expression 
without explicitly using the constructors -}
class ToExp a where
  toExp :: a -> Exp

instance ToExp Exp where
  toExp :: Exp -> Exp
  toExp = id

instance ToExp String where
  toExp = defaultSEQ . map CHAR

infixl 9 ^>
infixl 8 ?>
infixl 7 +>
infixl 6 *>
infixl 4 <~>
infixl 3 <|>
infixl 1 <$>

(<~>) :: (ToExp a, ToExp b) => a -> b -> Exp
a <~> b = defaultSEQ [toExp a, toExp b]
    -- case (toExp a, toExp b) of
    --     (SEQ _ xs, SEQ _ ys) -> defaultSEQ (xs ++ ys)
    --     (ae, be) -> defaultSEQ [ae,be]

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

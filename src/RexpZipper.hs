module RexpZipper where
import Test.QuickCheck

type Sym = Char

data Exp = ZERO
            | ONE
            | CHAR Char
            | SEQ Sym [Exp]
            | ALT [Exp]
            | STAR Exp [Exp] deriving (Ord, Eq, Show) -- the Exp it represents, the processed Exps
            -- | RECD [Char] Exp [Exp]

defaultSTAR :: Exp -> Exp
defaultSTAR r = STAR r []

data Context = TopC
            | SeqC Context Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Context -- Alternate has one context shared between its children
            | StarC Context [Exp] Exp deriving (Ord, Eq, Show)
            -- | RecdC Context Exp [Char] [Exp] 
data Zipper = Zipper Exp Context deriving (Ord, Eq, Show)

instance Arbitrary Exp where
    arbitrary = oneof
        [ return ZERO
        , return ONE
        , CHAR <$> arbitrary
        , SEQ <$> arbitrary <*> listOf arbitrary
        , ALT <$> listOf arbitrary
        , STAR <$> arbitrary <*> listOf arbitrary
        ]

focus :: Exp -> Zipper
focus r = Zipper r (SeqC TopC '\0' [] [r])

der ::  Char -> Zipper -> [Zipper]
der c (Zipper re ctx) = up re ctx
    where
    down :: Context -> Exp -> [Zipper]
    down _ ZERO = []
    down ct ONE
        | c == '\0' = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct (CHAR d)
        | c == d = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct r@(SEQ _ []) = up r ct
    down ct (SEQ s (e:es))= down (SeqC ct s [] es) e
    down ct (ALT es) = concatMap (down (AltC ct)) es
    down ct r@(STAR e es)
        | c == '\0' = up r ct
        | otherwise =
            let z = down (StarC ct es e) e in
                if null z then
                    up r ct
                else z
    -- down ct (RECD s r' es) = down (RecdC ct r' s es) r'
    up :: Exp -> Context -> [Zipper]
    up _ TopC = []
    up e (SeqC ct s es [])
        | c == '\0' && ct == TopC = 
            [Zipper (SEQ s (reverse(e:es))) ct]
        | otherwise = up (SEQ s (reverse(e:es))) ct
    up e (SeqC ct s el (er:esr)) = down (SeqC ct s (e:el) esr) er
    up e (AltC ct) = up (ALT [e]) ct
    up e (StarC ct es r)
        | c == '\0' =
            case ct of
                TopC -> [Zipper (STAR r (reverse (e:es))) ct]
                _ -> up (STAR r (reverse(e:es))) ct 
        | otherwise =
            let ct2 = StarC ct (e:es) r
                zs = down ct2 r in
                    if null zs then
                        up (STAR r (reverse (e:es))) ct
                    else zs
    -- up e (RecdC ct r s es)
    --     | c == '\0' = up (RECD s r [e]) ct 
    --     | otherwise = down (RecdC ct r s (e:es)) e

ders :: [Char] -> [Zipper] -> [Zipper]
ders cs zs = 
    let res = foldl (\ z c -> concatMap (der c) z) zs cs in
        der '\0' (head res)

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
flatten (SEQ c []) = [c]
flatten (SEQ s es)
    | s == '\0' = concatMap flatten es
    | otherwise = s:concatMap flatten es
flatten (ALT es) = concatMap flatten es
flatten (STAR _ es) = concatMap flatten es
-- flatten (RECD _ _ es) = concatMap flatten es

env :: Exp -> [([Char], [Char])]
env ZERO = error "ZERO is an invalid input for `env`"
env ONE = []
env (CHAR _) = []
env (ALT es) = concatMap env es
env (SEQ c es) = concatMap env es
env (STAR _ es) = concatMap env es
-- env (RECD s _ es) = (s, concatMap flatten es) : concatMap env es

-- plug :: Context -> Exp -> Exp
-- plug

-- lexZipper :: [Char] -> Exp -> Exp
-- lexZipper s r = 
--     let zs = ders s r in 
--         if null zs then
--             error "Could not lex"
--         else 
--             plug (head zs) r
-- lex :: [Char] -> Exp -> Token
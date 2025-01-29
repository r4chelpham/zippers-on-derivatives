module RexpZipper where

type Sym = Char

data Exp = CHAR Char
            | SEQ Sym [Exp]
            | ALT [Exp] deriving (Show)

data Context = TopC
            | SeqC Context Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Context deriving (Show) --
data Zipper = Zipper Exp Context deriving (Show)

focus :: Exp -> Zipper
focus r = Zipper r (SeqC TopC '\0' [] [r])

der ::  Char -> Zipper -> [Zipper]
der c (Zipper re ctx) = up re ctx
    where
    down :: Context -> Exp -> [Zipper]
    down ct (CHAR d)
        | c == d = [Zipper (SEQ c []) ct]
        | otherwise = []
    down ct r@(SEQ _ []) = up r ct
    down ct (SEQ s (e:es))= down (SeqC ct s [] es) e
    down ct (ALT es) = concatMap (down (AltC ct)) es
    up :: Exp -> Context -> [Zipper]
    up _ TopC = []
    up e (SeqC ct s es [])
        | c == '\0' = [Zipper (SEQ s (reverse (e:es))) ct]
        | otherwise = up  (SEQ s (reverse (e:es))) ct
    up e (SeqC ct s el (er:esr)) = down (SeqC ct s (e:el) esr) er
    up e (AltC ct) = up (ALT [e]) ct

ders :: [Char] -> [Zipper] -> [Zipper]
ders cs zs = foldl (\ z c -> concatMap (der c) z) zs (cs++['\0'])

matcher :: [Char] -> Exp -> Bool
matcher s r = not $ null $ ders s [focus r]
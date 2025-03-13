module ZipperLexer where

import RexpZipperv2
import Token
import qualified Data.Set as Set
import GHC.IORef

{- WHILE Language registers -}
keyword :: IO Exp
keyword = "while" <|> "if" <|> "then" <|> "else" <|> "do" <|> "for" <|>
          "to" <|> "true" <|> "false" <|> "read" <|> "write" <|>
          "skip" <|> "break"

op :: IO Exp
op = ">" <|> "<" <|> "==" <|> "!=" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||" <|> "+" <|> "-" <|> "*" <|> "%" <|> "/"

lett :: IO Exp
lett = toExp (RANGE $ Set.fromList (['A'..'Z'] ++ ['a'..'z']))

sym :: IO Exp
sym = do
    l <- lett
    -- ls <- newExp l
    l <|> RANGE (Set.fromList ['.', '_', '>', '<', '=', ';', ',', '\\', ':'])

parens :: IO Exp
parens = toExp (RANGE $ Set.fromList ['(', ')', '{', '}'])

digit :: IO Exp
digit = toExp (RANGE $ Set.fromList ['0'..'9'])

semi :: IO Exp
semi = toExp ";"

whitespace :: IO Exp
whitespace = (" " <|> "\n" <|> "\t" <|> "\r") RexpZipperv2.+> ()

identifier :: IO Exp
identifier = do
    ls <- lett
    -- letts <- newExp ls
    ds <- digit
    -- digits <- newExp ds
    ls <~> (("_" <|> ls <|> ds) RexpZipperv2.*> ())

numbers :: IO Exp
numbers = do
    "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> (digit RexpZipperv2.*> ()))

string :: IO Exp
string = do
    sms <- sym
    -- syms <- newExp sms
    ds <- digit
    -- digits <- newExp ds
    ps <- parens
    -- paren <- newExp ps
    ws <- whitespace
    -- wsps <- newExp ws
    "\"" <~> ((sms <|> ds <|> ps <|> ws <|> "\n") RexpZipperv2.*> ()) <~> "\""

eol :: IO Exp
eol = "\n" <|> "\r\n"

comment :: IO Exp
comment = do
    sms <- sym
    -- syms <- newExp sms
    ds <- digit
    -- digits <- newExp ds
    ps <- parens
    -- paren <- newExp ps
    e <- eol
    -- eols <- newExp e
    "//" <~> ((sms <|> ps <|> ds <|> toExp " " RexpZipperv2.*> ()) RexpZipperv2.*> ()) <~> e

whileRegs :: IO Exp
whileRegs = do
    kw <- keyword
    -- kws <- newExp kw
    o <- op
    -- os <- newExp o
    str <- string
    -- strs <- newExp str
    p <- parens
    -- ps <- newExp p
    s <- semi
    -- sc <- newExp s
    w <- whitespace
    -- ws <- newExp w
    i <- identifier
    -- ids <- newExp i
    n <- numbers
    -- ns <- newExp n
    c <- comment
    -- cs <- newExp c
    (("k" RexpZipperv2.<$> kw)
        <|> ("c" RexpZipperv2.<$> c)
        <|> ("o" RexpZipperv2.<$> o)
        <|> ("str" RexpZipperv2.<$> str)
        <|> ("p" RexpZipperv2.<$> p)
        <|> ("s" RexpZipperv2.<$> s)
        <|> ("w" RexpZipperv2.<$> w)
        <|> ("i" RexpZipperv2.<$> i)
        <|> ("n" RexpZipperv2.<$> n)) RexpZipperv2.*> ()


-- tokenise :: [Char] -> IO [Token]
-- tokenise s = do
--     whiles <- whileRegs
--     es <- run s whiles
--     es' <- mapM (readIORef . exp') es
--     esLexed <- concatMapM env es'
--     return (map token $ filter isNotWhitespace esLexed)
--   where isNotWhitespace ("w", _) = False
--         isNotWhitespace ("c", _) = False
--         isNotWhitespace _ = True

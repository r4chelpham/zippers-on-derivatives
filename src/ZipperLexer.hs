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
    ds <- digit
    RANGE (Set.fromList (['A'..'Z'] ++ ['a'..'z'])) <~> (("_" <|> ls <|> ds) RexpZipperv2.*> ())

numbers :: IO Exp
numbers = do
    "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> (digit RexpZipperv2.*> ()))

string :: IO Exp
string = do
    sms <- sym
    ds <- digit
    ps <- parens
    ws <- whitespace
    "\"" <~> ((sms <|> ds <|> ps <|> ws <|> "\n") RexpZipperv2.*> ()) <~> "\""

eol :: IO Exp
eol = "\n" <|> "\r\n"

comment :: IO Exp
comment = do
    sms <- sym
    ds <- digit
    ps <- parens
    e <- eol
    "//" <~> ((sms <|> ps <|> ds <|> toExp " " RexpZipperv2.*> ()) RexpZipperv2.*> ()) <~> e

whileRegs :: IO Exp
whileRegs = do
    kw <- keyword
    o <- op
    str <- string
    p <- parens
    s <- semi
    w <- whitespace
    i <- identifier
    n <- numbers
    c <- comment
    (("k" RexpZipperv2.<$> kw)
        <|> ("c" RexpZipperv2.<$> c)
        <|> ("o" RexpZipperv2.<$> o)
        <|> ("str" RexpZipperv2.<$> str)
        <|> ("p" RexpZipperv2.<$> p)
        <|> ("s" RexpZipperv2.<$> s)
        <|> ("w" RexpZipperv2.<$> w)
        <|> ("i" RexpZipperv2.<$> i)
        <|> ("n" RexpZipperv2.<$> n)) RexpZipperv2.*> ()

-- whileRegs :: IO [Exp]
-- whileRegs = do
--     kw <- keyword
--     o <- op
--     str <- string
--     p <- parens
--     s <- semi
--     w <- whitespace
--     i <- identifier
--     n <- numbers
--     c <- comment
--     return [kw, o, str, p, s, w, i, n, c]

-- tokenise :: Int -> [Char] -> IO [Token]
-- tokenise pos s = do
--     ws <- whileRegs -- | The rules that we need to match the input to


tokenise :: [Char] -> IO [Token]
tokenise s = do
    whiles <- whileRegs
    es <- run s whiles
    es' <- mapM (readIORef . exp') es
    esLexed <- concatMapM env es'
    return (map token $ filter isNotWhitespace esLexed)
  where isNotWhitespace ("w", _) = False
        isNotWhitespace ("c", _) = False
        isNotWhitespace _ = True

tokenise' :: Exp -> [Char] -> IO [Token]
tokenise' e s = do
    es <- run s e
    es' <- mapM (readIORef . exp') es
    esLexed <- concatMapM env es'
    return (map token $ filter isNotWhitespace esLexed)
  where isNotWhitespace ("w", _) = False
        isNotWhitespace ("c", _) = False
        isNotWhitespace _ = True
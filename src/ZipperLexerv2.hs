module ZipperLexerv2 where

import RexpZipperv2
import Token
import qualified Data.Set as Set

{- WHILE Language registers -}
keyword :: IO Rexp
keyword = "while" <|> "if" <|> "then" <|> "else" <|> "do" <|> "for" <|>
          "to" <|> "true" <|> "false" <|> "read" <|> "write" <|>
          "skip" <|> "break"

op :: IO Rexp
op = ">" <|> "<" <|> "==" <|> "!=" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||" <|> "+" <|> "-" <|> "*" <|> "%" <|> "/"

lett :: IO Rexp
lett = toRexp (RANGE $ Set.fromList (['A'..'Z'] ++ ['a'..'z']))

sym :: IO Rexp
sym = do
    l <- lett
    l <|> RANGE (Set.fromList ['.', '_', '>', '<', '=', ';', ',', '\\', ':'])

parens :: IO Rexp
parens = toRexp (RANGE $ Set.fromList ['(', ')', '{', '}'])

digit :: IO Rexp
digit = toRexp (RANGE $ Set.fromList ['0'..'9'])

semi :: IO Rexp
semi = toRexp ";"

whitespace :: IO Rexp
whitespace = (" " <|> "\n" <|> "\t" <|> "\r") RexpZipperv2.+> ()

identifier :: IO Rexp
identifier = do
    ls <- lett
    ds <- digit
    RANGE (Set.fromList (['A'..'Z'] ++ ['a'..'z'])) <~> (("_" <|> ls <|> ds) RexpZipperv2.*> ())

numbers :: IO Rexp
numbers = do
    "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> (digit RexpZipperv2.*> ()))

string :: IO Rexp
string = do
    sms <- sym
    ds <- digit
    ps <- parens
    ws <- whitespace
    "\"" <~> ((sms <|> ds <|> ps <|> ws <|> "\n") RexpZipperv2.*> ()) <~> "\""

eol :: IO Rexp
eol = "\n" <|> "\r\n"

comment :: IO Rexp
comment = do
    sms <- sym
    ds <- digit
    ps <- parens
    e <- eol
    "//" <~> ((sms <|> ps <|> ds <|> toRexp " " RexpZipperv2.*> ()) RexpZipperv2.*> ()) <~> e

whileRegs :: IO Rexp
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


tokenise :: [Char] -> IO [Token]
tokenise s = do
    whiles <- whileRegs
    es <- run s whiles
    case es of
        [] -> return []
        _ -> do
            res <- mapM env es
            let esLexed = head res
            return (map token $ filter isNotWhitespace esLexed)
  where isNotWhitespace ("w", _) = False
        isNotWhitespace ("c", _) = False
        isNotWhitespace _ = True

-- tokenise' :: Rexp -> [Char] -> IO [Token]
-- tokenise' e s = do
--     es <- run s e
--     es' <- mapM (readIORef . exp') es
--     esLexed <- concatMapM env es'
--     return (map token $ filter isNotWhitespace esLexed)
--   where isNotWhitespace ("w", _) = False
--         isNotWhitespace ("c", _) = False
--         isNotWhitespace _ = True
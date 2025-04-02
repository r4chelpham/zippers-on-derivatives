module ZipperLexer where

import RexpZipper
import Token
import qualified Data.Set as Set

{- WHILE Language registers - NOTE: doesn't work rn -}
keyword :: Rexp
keyword = "while" <|> "if" <|> "then" <|> "else" <|> "do" <|> "for" <|>
          "to" <|> "true" <|> "false" <|> "read" <|> "write" <|>
          "skip" <|> "break"

op :: Rexp
op = ">" <|> "<" <|> "==" <|> "!=" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||" <|> "+" <|> "-" <|> "*" <|> "%" <|> "/"

lett :: Rexp
lett = RANGE $ Set.fromList (['A'..'Z'] ++ ['a'..'z'])

sym :: Rexp
sym = lett <|> RANGE (Set.fromList ['.', '_', '>', '<', '=', ';', ',', '\\', ':'])

parens :: Rexp
parens = RANGE $ Set.fromList ['(', ')', '{', '}']

digit :: Rexp
digit = RANGE $ Set.fromList ['0'..'9']

semi :: Rexp
semi = toRexp ";"

whitespace :: Rexp
whitespace = (" " <|> "\n" <|> "\t" <|> "\r") RexpZipper.+> ()

identifier :: Rexp
identifier = lett <~> (("_" <|> lett <|> digit) RexpZipper.*> ())

numbers :: Rexp
numbers = "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> (digit RexpZipper.*> ()))

string :: Rexp
string = "\"" <~> ((sym <|> digit <|> parens <|> whitespace <|> "\n") RexpZipper.*> ()) <~> "\""

eol :: Rexp
eol = "\n" <|> "\r\n"

comment :: Rexp
comment = "//" <~> ((sym <|> parens <|> digit <|> toRexp " " RexpZipper.*> ()) RexpZipper.*> ()) <~> eol

whileRegs :: Rexp
whileRegs = (("k" RexpZipper.<$> keyword)
            <|> ("c" RexpZipper.<$> comment)
            <|> ("o" RexpZipper.<$> op)
            <|> ("str" RexpZipper.<$> string)
            <|> ("p" RexpZipper.<$> parens)
            <|> ("s" RexpZipper.<$> semi)
            <|> ("w" RexpZipper.<$> whitespace)
            <|> ("i" RexpZipper.<$> identifier)
            <|> ("n" RexpZipper.<$> numbers)) RexpZipper.*> ()


tokenise :: String -> [Token]
tokenise s =
  let res = lexSimp s whileRegs
  in 
    case res of 
      [] -> []
      _ -> map token $ filter isNotWhitespace (head res)
  where
    isNotWhitespace ("w", _) = False
    isNotWhitespace ("c", _) = False
    isNotWhitespace _ = True

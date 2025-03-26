module ZipperLexer where

import RexpZipper
import Token
import qualified Data.Set as Set

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

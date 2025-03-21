module ZipperLexer where

import RexpZipper
import Token
import qualified Data.Set as Set
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)

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
tokenise s =
  let allResults = lexSimp s whileRegs
      res = minimumBy (comparing length) allResults
  in map token $ filter isNotWhitespace res
  where
    isNotWhitespace ("w", _) = False
    isNotWhitespace ("c", _) = False
    isNotWhitespace _ = True

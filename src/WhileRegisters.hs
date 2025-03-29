module WhileRegisters where

import Rexp
import qualified Data.Set as Set

keyword :: Rexp
keyword = "while" <|> "if" <|> "then" <|> "else" <|> "do" <|> "for" <|> 
          "to" <|> "true" <|> "false" <|> "read" <|> "write" <|> 
          "skip" <|> "break"

op :: Rexp
op = "+" <|> "-" <|> "*" <|> "%" <|> "/" <|> "==" <|> "!=" <|> ">" <|> 
     "<" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||"

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
whitespace = PLUS (" " <|> "\n" <|> "\t" <|> "\r")

identifier :: Rexp
identifier = lett <~> STAR ("_" <|> lett <|> digit)

numbers :: Rexp
numbers = "0" <|> (RANGE (Set.fromList ['1'..'9']) <~> STAR digit)

string :: Rexp
string = "\"" <~> STAR (sym <|> digit <|> parens <|> whitespace <|> "\n") <~> "\""

eol :: Rexp
eol = "\n" <|> "\r\n"

comment :: Rexp
comment = "//" <~> STAR (sym <|> parens <|> digit <|> toRexp " ") <~> eol

whileRegs :: Rexp
whileRegs = Rexp.STAR (("k" Rexp.<$> keyword)
                <|> ("o" Rexp.<$> op)
                <|> ("str" Rexp.<$> string)
                <|> ("p" Rexp.<$> parens)
                <|> ("s" Rexp.<$> semi)
                <|> ("w" Rexp.<$> whitespace)
                <|> ("i" Rexp.<$> identifier)
                <|> ("n" Rexp.<$> numbers)
                <|> ("c" Rexp.<$> comment))
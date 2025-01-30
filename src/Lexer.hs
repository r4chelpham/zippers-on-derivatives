{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lexer where
import Rexp
import Val
import qualified Data.Set as Set

mkeps :: Rexp -> Val
mkeps ONE = Empty
mkeps (ALT r1 r2) =
    if nullable r1 then
        Val.Left (mkeps r1)
    else Val.Right (mkeps r2)
mkeps (SEQ r1 r2) = Sequ (mkeps r1) (mkeps r2)
mkeps (STAR _) = Stars []
mkeps (OPTIONAL _) = Opt Empty
mkeps (NTIMES _ 0) = NX []
mkeps (NTIMES r _) = NX [mkeps r]
mkeps (PLUS r) = Pls [mkeps r]
mkeps (RECD s r) = Rec s (mkeps r)

inj :: Rexp -> Char -> Val -> Val
inj r c v =
    case (r, v) of
       (STAR r1, Sequ v1 (Stars vs)) -> Stars (inj r1 c v1:vs)
       (SEQ r1 _, Sequ v1 v2) -> Sequ (inj r1 c v1) v2
       (SEQ r1 _, Val.Left (Sequ v1 v2)) -> Sequ (inj r1 c v1) v2
       (SEQ r1 _, Val.Right v2) -> Sequ (mkeps r1) (inj r1 c v2)
       (ALT r1 _, Val.Left v1) -> Val.Left (inj r1 c v1)
       (ALT _ r2, Val.Right v2) -> Val.Right (inj r2 c v2)
       (CHAR _, Empty) -> Chr c
       (RANGE _, Empty) -> Chr c
       (PLUS r1, Sequ v1 (Stars vs)) -> Pls (inj r1 c v1:vs)
       (OPTIONAL r1, _) -> Opt (inj r1 c v)
       (NTIMES r1 _, Sequ v1 (NX vs)) -> NX (inj r1 c v1:vs)
       (RECD s r1, _) -> Rec s (inj r1 c v)

-- simplification functions
fId :: Val -> Val
fId v = v

fRight :: (Val -> Val) -> (Val -> Val)
fRight f v = Val.Right (f v)

fLeft :: (Val -> Val) -> (Val -> Val)
fLeft f v = Val.Left (f v)

fAlt :: (Val -> Val) -> (Val -> Val) -> (Val -> Val)
fAlt f1 f2 v =
    case v of
        Val.Right v' -> Val.Right (f2 v')
        Val.Left v'  -> Val.Left (f1 v')

fSeq :: (Val -> Val) -> (Val -> Val) -> (Val -> Val)
fSeq f1 f2 v =
    case v of
        Sequ v1 v2 -> Sequ (f1 v1) (f2 v2)

fSeqEmpty1 :: (Val -> Val) -> (Val -> Val) -> (Val -> Val)
fSeqEmpty1 f1 f2 v = Sequ (f1 Empty) (f2 v)

fSeqEmpty2 :: (Val -> Val) -> (Val -> Val) -> (Val -> Val)
fSeqEmpty2 f1 f2 v = Sequ (f1 v) (f2 Empty)

fRecd :: (Val -> Val) -> (Val -> Val)
fRecd f v =
    case v of
        Rec x v' -> Rec x (f v')

fError :: Val -> Val
fError _ = error "error"

simp :: Rexp -> (Rexp, Val -> Val)
simp (ALT r1 r2) =
    let (r1s, f1s) = simp r1
        (r2s, f2s) = simp r2
    in case (r1s, r2s) of
        (ZERO, _) -> (r2s, fRight f2s)
        (_, ZERO) -> (r1s, fLeft f1s)
        _ ->
            (if r1s == r2s then
                (r1s, fLeft f1s)
            else
                (ALT r1s r2s, fAlt f1s f2s))
simp (SEQ r1 r2) =
    let (r1s, f1s) = simp r1
        (r2s, f2s) = simp r2
    in case (r1s, r2s) of
        (ZERO, _) -> (ZERO, fError)
        (_, ZERO) -> (ZERO, fError)
        (ONE, _) -> (r2s, fSeqEmpty1 f1s f2s)
        (_, ONE) -> (r1s, fSeqEmpty2 f1s f2s)
        _ -> (SEQ r1s r2s, fSeq f1s f2s)
simp r = (r, fId)

-- lexer functions
lexSimp :: Rexp -> String -> Val
lexSimp r []
    | nullable r = mkeps r
    | otherwise = error "Lexing error"
lexSimp r (c:cs) =
    let (rSimp, fSimp) = simp (der r c) in
    inj r c (fSimp $ lexSimp rSimp cs)

lexingSimp :: Rexp -> String -> [(String, String)]
lexingSimp r s = env $ lexSimp r s

keyword :: Rexp
keyword = "while" <|> "if" <|> "then" <|> "else" <|> "do" <|> "for" <|> "to" <|> "true" <|> "false" <|> "read" <|> "write" <|> "skip" <|> "break"

op :: Rexp
op = "+" <|> "-" <|> "*" <|> "%" <|> "/" <|> "==" <|> "!=" <|> ">" <|> "<" <|> "<=" <|> ">=" <|> ":=" <|> "&&" <|> "||"

lett :: Rexp
lett = RANGE (Set.fromList (['A' .. 'Z']++['a' .. 'z']))

sym :: Rexp
sym = lett <|> RANGE (Set.fromList ['.', '_', '>', '<', '=', ';', ',', '\\', ':'])

parens :: Rexp
parens = RANGE (Set.fromList ['(', ')', '{', '}'])

digit :: Rexp
digit = RANGE (Set.fromList ['1' .. '9'])

semi :: Rexp
semi = toRexp ";"

whitespace :: Rexp
whitespace = PLUS (" " <|> "\n" <|> "\t" <|> "\r")

id :: Rexp
id = lett <~> STAR ("_" <|> lett <|> digit)

num :: Rexp
num = "0" <|> STAR  (RANGE (Set.fromList ['1' .. '9']) <~> digit)

string :: Rexp
string = "\"" <~> STAR (sym <|> digit <|> parens <|> whitespace <|> "\n") <~> "\""

eol :: Rexp
eol = "\n" <|> "\r\n"

comment :: Rexp
comment = "//" <~> STAR (sym <|> parens <|> digit <|> STAR (toRexp " ")) <~> eol

data Token = T_KEYWORD String
            | T_OP String
            | T_STRING String
            | T_PAREN String
            | T_SEMI
            | T_ID String
            | T_NUM Integer

token :: (String, String) -> Token
token ("k", s) = T_KEYWORD s
token ("o", s) = T_OP s
token ("str", s) = T_STRING s
token ("p", s) = T_PAREN s
token ("s", _) = T_SEMI
token ("i", s) = T_ID s
token ("n", s) = T_NUM (read s :: Integer)

let WHILE_REGS = Rexp.STAR (("k" $ keyword) <|> 
                        ("o" $ op) <|> 
                        ("str" $ string) <|>
                        ("p" $ parens) <|>
                        ("s" $ semi) <|> 
                        ("w" $ whitespace) <|> 
                        ("i" $ id) <|> 
                        ("n" $ numbers) <|>
                        ("c" $ comment))

tokenise :: String -> [Token]
tokenise s = filter token $ lexingSimp WHILE_REGS s
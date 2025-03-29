module Token where

data Token = T_KEYWORD String
            | T_OP String
            | T_STRING String
            | T_PAREN String
            | T_SEMI
            | T_ID String
            | T_NUM String deriving (Show, Eq)

token :: ([Char], [Char]) -> Token
token ("k", s) = T_KEYWORD s
token ("o", s) = T_OP s
token ("str", s) = T_STRING s
token ("p", s) = T_PAREN s
token ("s", _) = T_SEMI
token ("i", s) = T_ID s
token ("n", s) = T_NUM s
token _ = error "Invalid token"

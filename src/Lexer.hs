{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lexer where
import Rexp
import Val

mkeps :: Rexp -> Val
mkeps ONE = Empty
mkeps (ALT r1 r2) = 
    if nullable r1 then 
        Val.Left (mkeps r1) 
    else Val.Right (mkeps r2)
mkeps (SEQ r1 r2) = Sequ (mkeps r1) (mkeps r2)
mkeps (STAR _) = Stars []
mkeps (RECD s r) = Rec s (mkeps r)

inj :: Rexp -> Char -> Val -> Val
inj (STAR r) c (Sequ v1 (Stars vs)) = Stars (inj r c v1:vs)
inj (SEQ r1 _) c (Sequ v1 v2) = Sequ (inj r1 c v1) v2
inj (SEQ r1 _) c (Val.Left (Sequ v1 v2)) = Sequ (inj r1 c v1) v2
inj (SEQ r1 _) c (Val.Right v2) = Sequ (mkeps r1) (inj r1 c v2)
inj (ALT r1 _) c (Val.Left v1) = Val.Left (inj r1 c v1)
inj (ALT _ r2) c (Val.Right v2) = Val.Right (inj r2 c v2)
inj (CHAR _) c Empty = Chr c
inj (RECD s r) c v = Rec s (inj r c v)

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
lexSimp :: Rexp -> [Char] -> Val
lexSimp r [] =
    if nullable r then
        mkeps r
    else
        error "Lexing error"
lexSimp r (c:cs) =
    let (rSimp, fSimp) = simp (der r c) in
        inj r c (fSimp $ lexSimp rSimp cs)

lexingSimp :: Rexp -> [Char] -> [([Char], [Char])]
lexingSimp r s = env $ lexSimp r s


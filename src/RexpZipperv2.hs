{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RexpZipperv2 where

import GHC.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Monad (foldM)
import Data.Ord (comparing)
import Data.List (intercalate)
import qualified Data.Set as Set

type Sym = Char

data Exp = Exp {
  mem :: IORef Mem
  , exp' :: IORef Exp'
}

instance Show Exp where
  show (Exp _ e') =
    "Mem: { ... }" ++
    "\nExp: " ++
    show (unsafePerformIO (readIORef e'))

instance Ord Exp where
    compare = comparing (unsafePerformIO . readIORef . exp')

instance Eq Exp where
  (Exp _ e1') == (Exp _ e2') = 
    unsafePerformIO (readIORef e1') == unsafePerformIO (readIORef e2')

{-| ZERO and ONE are not needed in the algorithm because they
  are represented as ALT [] and SEQ [] respectively.
-}
data Exp' = CHAR Char
            | SEQ Sym [Exp]
            | ALT [Exp]
            | STAR Exp
            | PLUS Exp
            | OPTIONAL Exp
            | NTIMES Int Exp
            | RECD [Char] Exp
            | RANGE (Set.Set Char)
            deriving (Ord, Eq, Show)

data Context = TopC
            | SeqC Mem Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Mem -- Alternate has one context shared between its children
            | StarC Mem [Exp] Exp'
            | PlusC Mem [Exp] Exp'
            | OptionalC Mem
            | NTimesC Mem Int [Exp] Exp'
            | RecdC Mem [Char]
            deriving (Ord, Eq, Show)

data Zipper = Zipper Exp' Mem deriving (Ord, Show)

instance Eq Zipper where
  (Zipper e m) == (Zipper e' m') =
    e == e' && m == m'

data Mem = Mem
  {
    start :: Int
    , end :: Int
    , parents :: IORef [Context]
    , result :: IORef Exp
  }

instance Eq Mem where
  (Mem s1 e1 ps1 r1) == (Mem s2 e2 ps2 r2) =
    s1 == s2 &&
    e1 == e2 &&
    unsafePerformIO (readIORef ps1) == unsafePerformIO (readIORef ps2) &&
    unsafePerformIO (readIORef r1) == unsafePerformIO (readIORef r2)

instance Ord Mem where
  compare = comparing start

instance Show Mem where
  show (Mem s e ps res) =
    "{Start: " ++
    show s ++
    "\nParents: " ++
    show (unsafePerformIO (readIORef ps)) ++
    "\nEnd: " ++
    show e ++
    "\nResult: " ++
    show (unsafePerformIO (readIORef res)) ++
    "}"

{-| Gives an Exp' an associated memory.

  Mainly called when we create an Exp from scratch - 
  often at the very beginning and not during the 
  actual matching/lexing of the Exp.
-}
createExp :: Exp' -> IO Exp
createExp e' = do
  m <- mBottom
  mRef <- newIORef m
  eSimp' <- simp e'
  eRef <- newIORef eSimp'
  return Exp
    {
      mem = mRef
      , exp' = eRef
    }

cloneAndCreate :: Exp -> IO Exp
cloneAndCreate e = do 
  e' <- (readIORef . exp') e
  eCloned' <- cloneExp' e'
  createExp eCloned'

cloneExp' :: Exp' -> IO Exp'
cloneExp' (CHAR c) = return (CHAR c)
cloneExp' (SEQ s []) = return (SEQ s [])
cloneExp' (ALT []) = return (ALT [])
cloneExp' (SEQ s es) = do
  esCloned <- mapM cloneAndCreate es
  return (SEQ s esCloned)
cloneExp' (ALT es) = do
  esCloned <- mapM cloneAndCreate es
  return (ALT esCloned)
cloneExp' (STAR e) = do
  eCloned <- cloneAndCreate e
  return (STAR eCloned)
cloneExp' (PLUS e) = do
  eCloned <- cloneAndCreate e
  return (PLUS eCloned)
cloneExp' (OPTIONAL e) = do
  eCloned <- cloneAndCreate e
  return (OPTIONAL eCloned)
cloneExp' (NTIMES n e) = do
  eCloned <- cloneAndCreate e
  return (NTIMES n eCloned)
cloneExp' (RECD s e) = do
  eCloned <- cloneAndCreate e
  return (RECD s eCloned)
cloneExp' (RANGE cs) = return (RANGE cs)

{-| Creates the default Expression and Memory to be
  used in the zipper operations.

  Equivalent to E⊥ and M⊥ in the Darragh and Adams paper
  respectively.
-}
defaults :: IO (Exp, Mem)
defaults = do
  mRef <- newIORef undefined
  e' <- newIORef (ALT [])
  ps <- newIORef [TopC]
  let e = Exp {
        mem = mRef
        , exp' = e'
      }
  eRef <- newIORef e
  let m = Mem {
          start = -1
          , end = -1
          , parents = ps
          , result = eRef
        }
  writeIORef mRef m
  return (e, m)

{-| Retrieve the default Exp.

  Equivalent to E⊥ in the Darragh and Adams paper.
-}
eBottom :: IO Exp
eBottom = fst Prelude.<$> defaults

{-| Retrieve the default Memory.

  Equivalent to M⊥ in the Darragh and Adams paper.
-}
mBottom :: IO Mem
mBottom = snd Prelude.<$> defaults

{-| Null character to signal the end of a lex/match.
-}
eof :: Char
eof = '\0'

{-| A function that finds the final matched value from the
  resultant lex/match.
-}
unwrapTopZipper :: Zipper -> IO Exp
unwrapTopZipper (Zipper _ m) = do
  ps <- readIORef (parents m)
  case ps of
    (SeqC m' '\0' (e:_) []:_) -> do
        ps' <- readIORef (parents m')
        if ps' == [TopC] then return e
        else error "parents doesn't meet the given structure"
    _ -> error "Top zipper is invalid"

{-| Creates a zipper that focuses on the given
  expression.
-}
focus :: Exp -> IO Zipper
focus e = do
    mTop <- mBottom
    writeIORef (parents mTop) [TopC]
    expEnd <- eBottom
    writeIORef (exp' expEnd) (CHAR '\0')
    let e' = SEQ '\0' []
    let root = SeqC mTop '\0' [] [e, expEnd]
    mSeq <- mBottom
    writeIORef (parents mSeq) [root]
    return (Zipper e' mSeq)

{-| Determines whether the expression given is able
  to match the empty string or not.
-}
nullable :: Exp' -> Bool
nullable (CHAR _) = False
nullable (ALT es) =
  any (\(Exp _ e') -> nullable (unsafePerformIO (readIORef e'))) es
nullable (SEQ _ []) = True
nullable (SEQ _ es) =
  all (\(Exp _ e') -> nullable (unsafePerformIO (readIORef e'))) es
nullable (STAR _) =  True
nullable (PLUS (Exp _ e')) = nullable (unsafePerformIO (readIORef e')) 
nullable (OPTIONAL _) = True
nullable (NTIMES 0 _) = True
nullable (NTIMES _ (Exp _ e')) = nullable (unsafePerformIO (readIORef e'))
nullable (RECD _ (Exp _ e')) = nullable (unsafePerformIO (readIORef e'))
nullable (RANGE _) = False

{- | Mainly used for when we need to pass down a new reference to 
  an exp to evaluate it a number of times (STAR, PLUS, NTIMES)

  We call this function when simplification is deemed unnecessary
  - to avoid excessive pruning.
-}
makeExp :: Exp' -> IO Exp
makeExp e' = do 
  eBott <- eBottom
  eRef <- newIORef e'
  let e'' = eBott {
    exp' = eRef
  }
  return e''

newExp :: Exp -> IO Exp
newExp e = do
  e' <- readIORef (exp' e)
  eCloned' <- cloneExp' e' 
  makeExp eCloned'

{-| The derivation function performed on a zipper
  wrt a character.

  It consists of 4 nested functions:
  up - 
  up' - Where the real movement of the zipper occurs at
    
  down - 
  down' - Where the "actual" derivation occurs at
    (Using Brzozowski's rules)

  Parameters:
  pos - Position at which the derivation occurs at.
  c - The character that the derivation is taken wrt.
  (Zipper ex me) - The zipper to take the derivation on.

  Returns:
  IO [Zipper] - A list of zippers with the result 
    of derivation.
-}
der ::  Int -> Char -> Zipper -> IO [Zipper]
der pos c (Zipper ex me) = up ex me
    where
    down :: Context -> Exp -> IO [Zipper]
    down ct e = do
      m <- readIORef (mem e)
      res <- readIORef (result m)
      if pos == start m then do
        modifyIORef (parents m) (++ [ct])
        if pos == end m
          then up' res ct
        else return []
      else do
        mBott <- mBottom
        let newMem = mBott {
          start = pos
        }
        writeIORef (parents newMem) [ct]
        writeIORef (mem e) newMem
        e' <- readIORef (exp' e)
        down' newMem e'

    down' :: Mem -> Exp' -> IO [Zipper]
    down' m (CHAR d)
        | c == d = return [Zipper (SEQ c []) m]
        | otherwise = return []
    down' m r@(SEQ _ []) = up r m
    down' m (SEQ s (e:es)) = do
      mBott <- mBottom
      let m' = mBott {
        start = start m
      }
      e' <- readIORef (exp' e)
      if nullable e' then do
        case es of
          [] -> do
            writeIORef (parents m') [AltC m]
            down (SeqC m' s [] []) e
          (el:esr) -> do
            writeIORef (parents m') [AltC m]
            z1 <- down (SeqC m' s [] es) e -- | Parse as normal...
            z2 <- down (SeqC m' s [el] esr) el 
            -- ^ Parse as if the first part matched the empty string...
            return (z1 ++ z2)
      else do
        writeIORef (parents m') [AltC m]
        down (SeqC m' s [] es) e
    down' m (ALT es) =
      foldM (\acc e -> do
        zs <- down (AltC m) e
        return (acc ++ zs)
        ) [] es
    down' m (STAR e) = do
      e' <- readIORef (exp' e)
      zs <- down (StarC m [] e') e
      if null zs then up (SEQ '\0' []) m
      else return zs
      -- e'' <- makeExp e'
      -- zs1 <- down (StarC m [] e') e
      -- mBott <- mBottom
      -- let m' = mBott {
      --   start = start m
      -- }
      -- writeIORef (parents m') [AltC m]
      -- zs2 <- down (StarC m' [] e') e''
      -- return (zs1 ++ zs2)
    down' m (PLUS e) = do
      e' <- readIORef (exp' e)
      down (PlusC m [] e') e
    down' m (OPTIONAL e) = do
      zs <- down (OptionalC m) e
      if null zs then up (SEQ '\0' []) m
      else return zs
      -- e' <- readIORef (exp' e)
      -- e'' <- makeExp e'
      -- zs1 <- down (OptionalC m) e
      -- mBott <- mBottom
      -- let m' = mBott {
      --   start = start m
      -- }
      -- writeIORef (parents m') [AltC m]
      -- zs2 <- down (OptionalC m') e''
      -- return (zs1 ++ zs2)
    down' _ (NTIMES 0 _) = return [] 
    down' m (NTIMES n e) = do
      e' <- readIORef (exp' e)
      down (NTimesC m (n-1) [] e') e
    down' m (RECD s e) = do
      down (RecdC m s) e
    down' m (RANGE cs)
      | Set.member c cs = return [Zipper (SEQ c []) m]
      | otherwise = return []

    up :: Exp' -> Mem -> IO [Zipper]
    up e' m = do
      e <- eBottom
      writeIORef (exp' e) e'
      let newMem = m {
          end = pos
      }
      writeIORef (mem e) newMem
      writeIORef (result m) e
      ps <- readIORef (parents m)
      concatMapM (up' e) ps

    up' :: Exp -> Context -> IO [Zipper]
    up' _ TopC = return []
    up' e (SeqC m s es []) = up (SEQ s (reverse (e:es))) m
    up' e (SeqC m s el (er:esr)) = down (SeqC m s (e:el) esr) er
    up' e (AltC m) = do
      if pos == end m then do
        res <- readIORef (result m)
        e' <- readIORef (exp' res)
        case e' of
          (ALT es) -> do
            _ <- writeIORef (exp' res) (ALT (es ++ [e]))
            return []
          _ -> error "Not an ALT "
      else do
        up (ALT [e]) m
    up' e (StarC m es e') = do
      let ct = StarC m (e:es) e'
      e'' <- makeExp e'
      zs <- down ct e''
      if null zs then do
        up (SEQ '\0' (reverse (e:es))) m 
      else return zs
    up' e (PlusC m es e') = do
      let ct = PlusC m (e:es) e'
      e'' <- makeExp e'
      zs <- down ct e''
      if null zs then do
        up (SEQ '\0' (reverse (e:es))) m
      else return zs
    up' e (OptionalC m) = up (SEQ '\0' [e]) m
    -- ^ up' on this case is called on a successful first-time match.
    up' e (NTimesC m 0 es _) = up (SEQ '\0' (reverse (e:es))) m
    up' e (NTimesC m n es e') = do
      let ct = NTimesC m (n-1) (e:es) e'
      e'' <- makeExp e'
      zs <- down ct e''
      if nullable e' then do
        mBott <- mBottom
        let m' = mBott {
          start = start m
        }
        writeIORef (parents m') [AltC m]
        zs' <- down (NTimesC m' (n-1) es e') e
        {- ^ Similar to the nullable case for SEQ , we have to check
          whether NTIMES can "skip" one iteration (i.e. it matches 
          the empty string. -}
        return (zs' ++ zs)
      else return zs
    up' e (RecdC m s) = up (RECD s e) m

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat Prelude.<$> mapM f xs

ders :: Int -> [Char] -> [Zipper] -> IO [Zipper]
ders pos [] zs = do
  concatMapM (der pos '\0') zs
ders pos (c:cs) zs = do
    newZs <- concatMapM (der pos c) zs
    ders (pos + 1) cs newZs

run :: [Char] -> Exp -> IO [Exp]
run [] e = do
  e' <- readIORef (exp' e)
  if nullable e' then do
    z@(Zipper _ m) <- focus e
    ps <- readIORef (parents m)
    let (SeqC m' s _ _) = head ps
    -- ^ ps will never be empty on a newly focused zipper.
    writeIORef (exp' e) (SEQ '\0' [])
    writeIORef (parents m) [SeqC m' s [e] []]
    res <- unwrapTopZipper z
    return [res]
  else return []
run s e = do
  z <- focus e
  zs <- ders 0 s [z]
  mapM unwrapTopZipper zs

{-| Takes a list of Exp derived from the `run` function
  and returns whether the operation was successful or not.
-}
matcher :: [Exp] -> Bool
matcher es = not (null es)

{-| Flattens an Exp' into a String.

  Based off of Urban's `flatten` from his method of lexing
  with derivatives.

  The reason why the `flatten` method does not have
  extensive matching is because in all terminal cases of `up'`,
  we return a SEQ, ALT or RECD when going back up the zipper, 
  so there is no need to match any other Exp'
-}
flatten :: Exp' -> IO [Char]
flatten (SEQ s []) = do
  if s == '\0' then return [] else return [s]
flatten (SEQ _ es) = do
  es' <- concatMapM getExp' es
  concatMapM flatten es'
flatten (ALT es) = do
  es' <- concatMapM getExp' es
  concatMapM flatten es'
flatten (RECD _ e) = do
  e' <- readIORef (exp' e)
  flatten e'
flatten _ = return ['\0']

getExp' :: Exp -> IO [Exp']
getExp' (Exp _ eRef') = do
  e' <- readIORef eRef'
  case e' of
    (CHAR _) -> return [e']
    (RANGE _) -> return [e']
    (SEQ _ []) -> return [e']
    (SEQ _ es) -> concatMapM getExp' es
    (ALT es) -> concatMapM getExp' es
    (STAR e) -> getExp' e
    (PLUS e) -> getExp' e
    (OPTIONAL e) -> getExp' e
    (NTIMES _ e) -> getExp' e
    (RECD _ e) -> getExp' e

{-| Returns a list of (String, String) pairs that will be used
  for tokenising.

  Based off of Urban's `env` from his method of lexing
  with derivatives.

  Like `flatten`, the reason why `env` does not have
  extensive matching is because in all terminal cases of `up'`,
  we return a SEQ, ALT or RECD when going back up the zipper, 
  so there is no need to match any other Exp'
-}
env :: Exp' -> IO [([Char], [Char])]
env (SEQ _ []) = return []
env (SEQ _ es) = process es -- | Continue looking for a RECD...
env (ALT es) = process es -- | Continue looking for a RECD...
env (RECD s e) = do
  e' <- readIORef (exp' e)
  v <- flatten e'
  vs <- env e' 
  return ((s, v):vs)
env _ = return []

process :: [Exp] -> IO [([Char], [Char])]
process [] = return []
process (e:es) = do
  e' <- readIORef (exp' e)
  case e' of
    recd@(RECD _ _) -> env recd 
    (SEQ _ es') -> do 
      rs <- process es'
      rs' <- process es
      return (rs ++ rs')
    (ALT es') -> do
      rs <- process es'
      rs' <- process es
      return (rs ++ rs')
    _ -> process es


-- plug :: Exp -> Context -> Zipper
-- plug e (SeqC TopC _ _ _) = Zipper e TopC
-- plug e (SeqC ct s es _) = plug (SEQ s (reverse(e:es))) ct
-- plug e (AltC ct) = plug e ct
-- plug _ _ = error "could not plug"

-- | Simplifications for Exps to avoid unnecessary node creation

isEmptySeq :: Exp' -> Bool
isEmptySeq (SEQ _ []) = True
isEmptySeq _          = False

simp :: Exp' -> IO Exp'
simp (SEQ s es) = do
  esFlattened <- flattenSEQ es
  es'' <- mapM (readIORef . exp') esFlattened
  es' <- mapM simp es''

  if ALT [] `elem` es'
  then return (ALT [])
  else do
    let filteredEs = filter (not . isEmptySeq) es'
    esSimp <- mapM createExp (filter (not . isEmptySeq) filteredEs)    
    return (SEQ s esSimp)
  where
    flattenSEQ :: [Exp] -> IO [Exp]
    flattenSEQ [] = return []
    flattenSEQ (e:es) = do
      e' <- readIORef (exp' e)
      case e' of 
        (SEQ s' es') | s == s' -> do
          esFlat' <- flattenSEQ es'
          esFlat <- flattenSEQ es
          return (esFlat' ++ esFlat)
        _ -> do
          esFlat <- flattenSEQ es
          return (e:esFlat)

simp (ALT es) = do
  esFlattened <- flattenALT es
  es'' <- mapM (readIORef . exp') esFlattened
  es' <- mapM simp es''
  let filteredEs = filter (/= ALT []) es'
  esSimp <- mapM createExp (filter (/= ALT []) filteredEs)
  return (ALT esSimp)
  where
    flattenALT :: [Exp] -> IO [Exp]
    flattenALT [] = return []
    flattenALT (e:es) = do
      e' <- readIORef (exp' e)
      case e' of 
        (ALT es') -> do
          esFlat' <- flattenALT es'
          esFlat <- flattenALT es
          return (esFlat' ++ esFlat)
        _ -> do
          esFlat <- flattenALT es
          return (e:esFlat)

simp e' = return e'

-- | String extensions to make creation of Exps easier.
stringToExp' :: [Char] -> IO Exp'
stringToExp' [] = return (SEQ '\0' [])
stringToExp' [c] = return (CHAR c)
stringToExp' (c:cs) = do
  e <- stringToExp [c]
  es <- mapM (createExp . CHAR) cs
  return (SEQ '\0' (e:es))

stringToExp :: [Char] -> IO Exp
stringToExp s = do
  e' <- stringToExp' s
  createExp e'

class ToExp' a where
  toExp' :: a -> IO Exp'

instance ToExp' (IO Exp') where
  toExp' :: IO Exp' -> IO Exp'
  toExp' = id

instance ToExp' Exp' where
  toExp' :: Exp' -> IO Exp'
  toExp' = return

instance ToExp' Exp where
  toExp' :: Exp -> IO Exp'
  toExp' e = readIORef (exp' e)

instance ToExp' (IO Exp) where
  toExp' :: IO Exp -> IO Exp'
  toExp' eIO = do
    e <- eIO
    readIORef (exp' e)

instance ToExp' String where
  toExp' = stringToExp'


class ToExp a where
  toExp :: a -> IO Exp

instance ToExp Exp where
  toExp :: Exp -> IO Exp
  toExp = return

instance ToExp (IO Exp) where
  toExp :: IO Exp -> IO Exp
  toExp = id

instance ToExp (IO Exp') where
  toExp :: IO Exp' -> IO Exp
  toExp eIO' = do
    e' <- eIO'
    createExp e'

instance ToExp Exp' where
  toExp :: Exp' -> IO Exp
  toExp = createExp

instance ToExp String where
  toExp = stringToExp

infixl 9 ^>
infixl 8 ?>
infixl 7 +>
infixl 6 *>
infixl 4 <~>
infixl 3 <|>
infixl 1 <$>

(<~>) :: (ToExp a, ToExp b) => a -> b -> IO Exp
a <~> b = do
  ae <- toExp a
  be <- toExp b
  createExp (SEQ '\0' [ae,be])

(<|>) :: (ToExp a, ToExp b) => a -> b -> IO Exp
a <|> b = do
  ae <- toExp a
  be <- toExp b
  createExp (ALT [ae, be])

(<$>) :: ToExp a => String -> a -> IO Exp
s <$> a = do
  e <- toExp a
  createExp (RECD s e)

(*>) :: ToExp a => a -> b -> IO Exp
a *> _ = do
  e <- toExp a
  createExp (STAR e)

(+>) :: ToExp a => a -> b -> IO Exp
a +> _ = do
  e <- toExp a
  createExp (PLUS e)

(?>) :: ToExp a => a -> b -> IO Exp
a ?> _ = do
  e <- toExp a
  createExp (OPTIONAL e)

(^>) :: ToExp a => a -> Int -> IO Exp
a ^> n = do
  e <- toExp a
  createExp (NTIMES n e)

-- | pretty-printing (ish) Zippers
implode :: [[Char]] -> [Char]
implode = intercalate "\n"

explode :: [Char] -> [[Char]]
explode = lines

lst :: [Char] -> [Char]
lst s = case explode s of
    []   -> ""
    h:tl -> implode $ (" └" ++ h) : map ("  " ++) tl

mid :: [Char] -> [Char]
mid s = case explode s of
    []   -> ""
    h:tl -> implode $ (" ├" ++ h) : map (" │" ++) tl

indent :: [[Char]] -> [Char]
indent [] = ""
indent ss = implode $ map mid (init ss) ++ [lst (last ss)]

pps :: [Exp] -> String
pps es = indent (map ppe es)

ppe :: Exp -> String
ppe (Exp _ e') =
  "EXP\n" ++
  indent ("Mem: { ... }" : [ppe' (unsafePerformIO (readIORef e'))])

ppe' :: Exp' -> String
ppe' (CHAR c)     = c : "\n"
ppe' (SEQ s es)  =  (if null es then "SEQ "++[s] else "SEQ\n" ++ pps es) ++ "\n"
ppe' (ALT es) = (if null es then "ALT []\n" else "ALT\n"++pps es) ++ "\n"
ppe' (RANGE cs) = Set.showTreeWith True False cs ++ "\n"
ppe' (STAR e) = "STAR\n" ++ indent [ppe e]
ppe' (PLUS e) = "PLUS\n" ++ indent [ppe e]
ppe' (OPTIONAL e) = "OPTIONAL\n" ++ indent [ppe e]
ppe' (NTIMES n e) = "NTIMES " ++ show n ++ "\n" ++ indent [ppe e]
ppe' (RECD s e) = "RECD " ++ s ++ "\n" ++ indent [ppe e]


ppz :: Zipper -> String
ppz (Zipper e' m) = "ZIP\n" ++ indent (ppe' e':[ppm m])

ppm :: Mem -> String
ppm (Mem st en ps res) =
  "MEM\n" ++
  "Start: " ++ show st ++ "\n" ++
  "End: " ++ show en ++ "\n" ++
  "Parents:\n" ++
  indent (map ppctx (unsafePerformIO (readIORef ps))) ++ "\n" ++
  "Result:\n" ++
  indent [ppe (unsafePerformIO (readIORef res))]

ppctx :: Context -> String
ppctx TopC = "TOP\n"
ppctx (SeqC m _ _ _) = "SEQC\n" ++ indent [ppm m]
ppctx (AltC m) = "ALTC\n" ++ indent [ppm m]
ppctx (StarC m es e) = "STARC\n" ++ indent [ppm m]
ppctx (PlusC m es e) = "PLUSC\n" ++ indent [ppm m]
ppctx (OptionalC m) = "OPTIONALC\n" ++ indent [ppm m]
ppctx (NTimesC m n es e) = "NTIMESC " ++ show n ++ "\n" ++ indent [ppm m]
ppctx (RecdC m s) = "RECDC " ++ s ++ "\n" ++ indent [ppm m]
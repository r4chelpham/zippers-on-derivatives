module RexpZipperv2 where

import GHC.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Monad (foldM)
import Data.Ord (comparing)
import Data.List (intercalate)

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
  (Exp m1 e1') == (Exp m2 e2') =
    m1 == m2 && e1' == e2'

data Exp' = ZERO
            | ONE
            | CHAR Char
            | SEQ Sym [Exp]
            | ALT [Exp]
            | STAR Exp
            | PLUS Exp
            | OPTIONAL Exp
            | NTIMES Int Exp
            deriving (Ord, Eq, Show)

data Context = TopC
            | SeqC Mem Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Mem -- Alternate has one context shared between its children
            | StarC Mem [Exp] Exp'
            | PlusC Mem [Exp] Exp'
            | OptionalC Mem [Exp]
            | NTimesC Mem Int [Exp] Exp'
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
    ps1 == ps2 &&
    r1 == r2

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

{- 
  TODO: fix so that it creates Exps within the Exp'
  (e.g. SEQ, ALT)
-}
createExp :: Exp' -> IO Exp
createExp e' = do
  m <- mBottom
  mRef <- newIORef m
  eRef <- newIORef e'
  return Exp
    {
      mem = mRef
      , exp' = eRef
    }

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

eBottom :: IO Exp
eBottom = fst <$> defaults

mBottom :: IO Mem
mBottom = snd <$> defaults

{-
  Null character to signal the end of a lex/match.
-}
eof :: Char
eof = '\0'

{-
  A function that finds the final matched value from the
  resultant lex/match.
-}
unwrapTopZipper :: Zipper -> IO Exp
unwrapTopZipper (Zipper _ m) = do
  ps <- readIORef (parents m)
  case ps of
    [SeqC m' '\0' (e:_) []] -> do
        ps' <- readIORef (parents m')
        if ps' == [TopC] then return e
        else error "Top zipper is invalid"
    _ -> error "Top zipper is invalid"


{- the zippers created are not the same but the references to e are? -}
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

nullable :: Exp' -> Bool
nullable ZERO = False
nullable ONE = True
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

{- | Mainly used for when we need to pass down a new reference to 
an exp to evaluate it a number of times (STAR, PLUS, NTIMES)
-}
newExp :: Exp' -> IO Exp
newExp e = do 
  eBott <- eBottom
  eRef <- newIORef e
  let e'' = eBott {
    exp' = eRef
  }
  return e''

der ::  Int -> Char -> Zipper -> IO [Zipper]
der pos c (Zipper re me) = up re me
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
    down' _ ZERO = return []
    down' _ ONE = return []
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
            down (SeqC m' s [] es) e
          (el:esr) -> do
            writeIORef (parents m') [AltC m]
            z1 <- down (SeqC m' s [] es) e -- | Parse as normal...
            z2 <- down (SeqC m' s [] esr) el -- | Parse as if the first part matched the empty string...
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
      mBott <- mBottom
      e' <- readIORef (exp' e)
      let m' = mBott {
        start = start m
      }
      let ct = StarC m [] e'
      writeIORef (parents m') [ct]
      down ct e
    down' m (PLUS e) = do
      mBott <- mBottom
      e' <- readIORef (exp' e)
      let m' = mBott {
        start = start m 
      }
      let ct = PlusC m [] e'
      writeIORef (parents m') [ct]
      down ct e
    down' m (OPTIONAL e) = down (OptionalC m []) e
    down' _ (NTIMES 0 _) = return [] 
    down' m (NTIMES n e) = do
      mBott <- mBottom
      e' <- readIORef (exp' e)
      let m' = mBott {
        start = start m
      }
      let ct = NTimesC m (n-1) [] e'
      writeIORef (parents m') [ct]
      down ct e

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
      e'' <- newExp e'
      zs <- down ct e''
      if null zs then do
        up (SEQ '\0' (reverse (e:es))) m 
      else return zs
    up' e (PlusC m es e') = do
      let ct = PlusC m (e:es) e'
      e'' <- newExp e'
      zs <- down ct e''
      if null zs then do
        up (SEQ '\0' (reverse (e:es))) m
      else return zs
    up' e (OptionalC m es) = up (SEQ '\0' (reverse (e:es))) m
    -- ^ up' on this case is called on a successful first-time match.
    up' e (NTimesC m 0 es _) = up (SEQ '\0' (reverse (e:es))) m
    up' e (NTimesC m n es e') = do
      let ct = NTimesC m (n-1) (e:es) e'
      e'' <- newExp e'
      zs <- down ct e''
      if null zs && nullable e' then do
          emptyStr <- createExp (SEQ '\0' [])
          down ct emptyStr 
      else return zs

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

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
    let (SeqC m' s _ _) = head ps -- ps will never be empty on a newly focused zipper
    writeIORef (exp' e) (SEQ '\0' [])
    writeIORef (parents m) [SeqC m' s [e] []]
    res <- unwrapTopZipper z
    return [res]
  else return []
run s e = do
  z <- focus e
  zs <- ders 0 s [z]
  mapM unwrapTopZipper zs

{-
  Takes a list of Exp derived from the `run` function
  and returns whether the operation was successful or not.
-}
matcher :: [Exp] -> Bool
matcher es = not (null es)

-- plug :: Exp -> Context -> Zipper
-- plug e (SeqC TopC _ _ _) = Zipper e TopC
-- plug e (SeqC ct s es _) = plug (SEQ s (reverse(e:es))) ct
-- plug e (AltC ct) = plug e ct
-- plug _ _ = error "could not plug"

-- pretty-printing (ish) Zippers
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
ppe' ZERO        = "0\n"
ppe' ONE         = "1\n"
ppe' (CHAR c)     = c : "\n"
ppe' (SEQ s es)  =  (if null es then "SEQ "++[s] else "SEQ\n" ++ pps es) ++ "\n"
ppe' (ALT es) = (if null es then "ALT []\n" else "ALT\n"++pps es) ++ "\n"

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
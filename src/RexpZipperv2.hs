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
            deriving (Ord, Eq, Show)

data Context = TopC
            | SeqC Mem Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Mem -- Alternate has one context shared between its children
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
  returns a different exp each time, 
  but changes are being made to that one exp each time 
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

-- | null character to indicate the end of a string and to go back up
eof :: Char
eof = '\0'

cloneExp :: Exp -> IO Exp
cloneExp original = do

    originalMem <- readIORef (mem original)

    originalExp' <- readIORef (exp' original)
    -- when checking originalExp', you need to clone the inside of the exps as well

    newMemRef <- newIORef originalMem
    newExp'Ref <- newIORef originalExp'

    return Exp
      { mem = newMemRef
      , exp' = newExp'Ref
      }

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

-- isNullable :: Context -> Bool
-- isNullable TopC = False
-- isNullable (AltC ct) = isNullable ct
-- isNullable (SeqC TopC _ _ _) = True
-- isNullable (SeqC ct _ _ es) = Prelude.null es && isNullable ct

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
      writeIORef (parents m') [AltC m]
      down (SeqC m' s [] es) e
    down' m (ALT es) =
      foldM (\acc e -> do
        zs <- down (AltC m) e
        return (acc ++ zs)
        ) [] es

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
      parentResults <- mapM (up' e) ps
      return $ concat parentResults

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

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

ders :: Int -> [Char] -> [Zipper] -> IO [Zipper]
ders pos [] zs = do
  concatMapM (der pos '\0') zs
ders pos (c:cs) zs = do
    newZs <- concatMapM (der pos c) zs
    ders (pos + 1) cs newZs

run :: [Char] -> Exp -> IO [Zipper]
run s e = do
  z <- focus e
  ders 0 s [z]

-- matcher :: [Char] -> Exp -> IO Bool
-- matcher [] r = return $ nullable r
-- matcher s r = do
--     let initialZipper = focus r
--     zs <- ders 0 s [initialZipper]
--     return $ not (Prelude.null zs) && any (\(Zipper _ ct) -> isNullable ct) zs

-- plug :: Exp -> Context -> Zipper
-- plug e (SeqC TopC _ _ _) = Zipper e TopC
-- plug e (SeqC ct s es _) = plug (SEQ s (reverse(e:es))) ct
-- plug e (AltC ct) = plug e ct
-- plug _ _ = error "could not plug"

-- pretty-printing REGs
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
ppctx (SeqC m _ el er) = "SEQC\n" ++ indent [ppm m] 
ppctx (AltC m) = "ALTC\n" ++ indent [ppm m]
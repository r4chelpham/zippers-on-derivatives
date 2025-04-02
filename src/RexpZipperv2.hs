{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module RexpZipperv2 where

import GHC.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Ord (comparing)
import Data.List (intercalate)
import qualified Data.Set as Set

type Sym = Char

data Rexp = Rexp {
  mem :: IORef Mem
  , rexp' :: IORef Rexp'
}

instance Show Rexp where
  show (Rexp _ r') =
    "Mem: { ... }" ++
    "\nRexp: " ++
    show (unsafePerformIO (readIORef r'))

instance Ord Rexp where
    compare = comparing (unsafePerformIO . readIORef . rexp')

instance Eq Rexp where
  (Rexp _ r1') == (Rexp _ r2') = 
    unsafePerformIO (readIORef r1') == unsafePerformIO (readIORef r2')

{-| ZERO and ONE are not needed in the algorithm because they
  are represented as ALT [] and SEQ [] respectively.
-}
data Rexp' = CHAR Char
            | SEQ Sym [Rexp]
            | ALT [Rexp]
            | STAR Rexp
            | PLUS Rexp
            | OPTIONAL Rexp
            | NTIMES Int Rexp
            | RECD [Char] Rexp
            | RANGE (Set.Set Char)
            deriving (Ord, Eq, Show)

data Context = TopC
            | SeqC Mem Sym [Rexp] [Rexp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Mem -- Alternate has one context shared between its children
            | StarC Mem [Rexp] Rexp
            -- | PlusC Mem [Rexp] Rexp
            | OptionalC Mem
            -- | NTimesC Mem Int [Rexp] Rexp Bool
            | RecdC Mem [Char]
            deriving (Ord, Eq, Show)

data Zipper = Zipper Rexp' Mem deriving (Ord, Show)

instance Eq Zipper where
  (Zipper e m) == (Zipper e' m') =
    e == e' && m == m'

data Mem = Mem
  {
    start :: Int
    , end :: IORef Int
    , parents :: IORef [Context]
    , result :: IORef Rexp
  }

instance Eq Mem where
  (==) :: Mem -> Mem -> Bool
  (Mem s1 e1 ps1 r1) == (Mem s2 e2 ps2 r2) =
    s1 == s2 &&
    unsafePerformIO (readIORef e1) == unsafePerformIO (readIORef e2) &&
    unsafePerformIO (readIORef ps1) == unsafePerformIO (readIORef ps2) &&
    unsafePerformIO (readIORef r1) == unsafePerformIO (readIORef r2)

instance Ord Mem where
  compare m1 m2 = compare (start m1) (start m2)

instance Show Mem where
  show (Mem s e ps res) =
    "{Start: " ++
    show s ++
    "\nParents: " ++
    show (unsafePerformIO (readIORef ps)) ++
    "\nEnd: " ++
    show (unsafePerformIO (readIORef e)) ++
    "\nResult: " ++
    show (unsafePerformIO (readIORef res)) ++
    "}"


count :: IORef Integer
{-# NOINLINE count #-}
count = unsafePerformIO $ newIORef 0

{-| Gives an Rexp' an associated memory.

  Mainly called when we create an Rexp from scratch - 
  often at the very beginning and not during the 
  actual matching/lexing of the Rexp.
-}
createRexp :: Rexp' -> IO Rexp
createRexp e' = do
  m <- mBottom
  mRef <- newIORef m
  eSimp' <- simp e'
  eRef <- newIORef eSimp'
  return Rexp
    {
      mem = mRef
      , rexp' = eRef
    }

{-| Creates the default Rexp and Memory to be
  used in the zipper operations.

  Equivalent to E⊥ and M⊥ in the Darragh and Adams paper
  respectively.
-}
defaults :: IO (Rexp, Mem)
defaults = do
  mRef <- newIORef undefined
  e' <- newIORef (ALT [])
  ps <- newIORef [TopC]
  end' <- newIORef (-1)
  let e = Rexp {
        mem = mRef
        , rexp' = e'
      }
  eRef <- newIORef e
  let m = Mem {
          start = -1
          , end = end'
          , parents = ps
          , result = eRef
        }
  writeIORef mRef m
  return (e, m)

{-| Retrieve the default Rexp.

  Equivalent to E⊥ in the Darragh and Adams paper.
-}
rBottom :: IO Rexp
rBottom = fst Prelude.<$> defaults

{-| Retrieve the default Memory.

  Equivalent to M⊥ in the Darragh and Adams paper.
-}
mBottom :: IO Mem
mBottom = snd Prelude.<$> defaults

{-| Null character to signal the end of a lex/match.
-}
cBottom :: Char
cBottom = '\0'

{-| Placeholder for the symbol of a SEQ.
-}
sBottom :: Char
sBottom = '\0'

{-| Creates a zipper that focuses on the given
  expression.
-}
focus :: Rexp -> IO Zipper
focus e = do
    mTop <- mBottom
    writeIORef (parents mTop) [TopC]
    let e' = SEQ sBottom []
    let root = SeqC mTop sBottom [] [e]
    mSeq <- mBottom
    writeIORef (parents mSeq) [root]
    return (Zipper e' mSeq)

{-| Determines whether the expression given is able
  to match the empty string or not.
-}
nullable :: Rexp -> IO Bool
nullable r = do
  r' <- readIORef (rexp' r)
  case r' of
    (CHAR _) -> do
      modifyIORef' count (+1)
      return False
    (ALT rs) -> do
      modifyIORef' count (+1)
      -- res <- mapM nullable es
      -- return (or res)
      process id False (||) rs
    (SEQ _ []) -> do
      modifyIORef' count (+1)
      return True
    (SEQ _ rs) -> do
      modifyIORef' count (+1)
      -- res <- mapM nullable es
      -- return (and res)
      process not True (&&) rs
    (STAR _) -> do
      modifyIORef' count (+1)
      return True
    (PLUS r'') -> do
      modifyIORef' count (+1)
      nullable r''
    (OPTIONAL _) -> do
      modifyIORef' count (+1)
      return True
    (NTIMES 0 _) -> do
      modifyIORef' count (+1)
      return True
    (NTIMES _ r'') -> do
      modifyIORef' count (+1)
      nullable r''
    (RECD _ r'') -> do
      modifyIORef' count (+1)
      nullable r''
    (RANGE _) -> do
      modifyIORef' count (+1)
      return False
  where
    {-| Helper function to process lists of expressions.
      condition - The condition for nullability (
        ALT - `id` because a nullable child makes ALT nullable.
        SEQ - `not` because a non-nullable child makes SEQ non-nullable.
      )
      identity - The accumulated result of the previous elements in the list. 
      combine - The operator to perform the accumulation of the results on (
        ALT - (||) OR operator because nullable (ALT [e1 .. en]) = nullable e1 || .. || nullable en
        ALT - (&&) AND operator because nullable (SEQ [e1 .. en]) = nullable e1 && .. && nullable en
      )
      (r:rs) - The list of elements to process.
    -}
    process :: (Bool -> Bool) -> Bool -> (Bool -> Bool -> Bool) -> [Rexp] -> IO Bool
    process _ identity _ [] = return identity
    process condition identity combine (r:rs) = do
      b <- nullable r
      if condition b
        then return b
        else do
          rest <- process condition identity combine rs
          return (combine b rest)

{-| Global variable - workList holds the zippers created from a
  derivation at a time.
-}
workList :: IORef [Zipper]
{-# NOINLINE workList #-}
workList = unsafePerformIO $ newIORef []

{-| Global variable - tops holds the regexes obtained from
  reaching the root of any zipper.
-}
tops :: IORef [Rexp]
{-# NOINLINE tops #-}
tops = unsafePerformIO $ newIORef []

{-| The derivation function performed on a zipper
  wrt a character.

  It consists of 4 nested functions:
  up - Prepares the zipper to go up each parent of a regex
  up' - Where the zipper tries to move upward
    
  down - Looks for shared expressions to avoid recomputation 
    of derivatives
  down' - Where the "actual" derivation occurs at
    (Using Brzozowski's rules)

  Parameters:
  pos - Position at which the derivation occurs at.
  c - The character that the derivation is taken wrt.
  (Zipper ex me) - The zipper to take the derivation on.
-}
der ::  Int -> Char -> Zipper -> IO ()
der pos c (Zipper re me) = up re me
    where
    down :: Context -> Rexp -> IO ()
    down ct r = do
      m <- readIORef (mem r)
      if pos == start m then do
        modifyIORef (parents m) (++ [ct])
        end' <- readIORef (end m)
        if pos == end'
          then do
            res <- readIORef (result m)
            up' res ct
        else return ()
      else do
        mBott <- mBottom
        let newMem = mBott {
          start = pos
        }
        writeIORef (parents newMem) [ct]
        writeIORef (mem r) newMem
        r' <- readIORef (rexp' r)
        down' newMem r'

    down' :: Mem -> Rexp' -> IO ()
    down' m (CHAR d)
        | c == d = do
          modifyIORef' count (+1)
          modifyIORef workList (++ [Zipper (SEQ c []) m])
        | otherwise = modifyIORef' count (+1)
    down' m r@(SEQ _  []) = do
        modifyIORef' count (+1)
        up r m
    down' m (SEQ s (r:rs)) = do
      nu <- nullable r
      modifyIORef' count (+1)
      if nu then do
        mBott <- mBottom
        let m' = mBott {
          start = start m
        }
        writeIORef (parents m') [AltC m]
        down (SeqC m' s [] rs) r
        case rs of
          [] -> return () -- | Derive as normal...
          (r':rs') -> down (SeqC m' s [] rs') r'
          -- ^ Derive as if the first part matched the empty string...
      else down (SeqC m s [] rs) r
    down' m (ALT rs) = do
      modifyIORef' count (+1)
      mapM_ (down (AltC m)) rs
    down' m (STAR r) = do
      modifyIORef' count (+1)
      down (StarC m [] r) r
    down' m (PLUS r) = do
      modifyIORef' count (+1)
      down (StarC m [] r) r
    down' m (OPTIONAL r) = do
      modifyIORef' count (+1)
      down (OptionalC m) r
    down' m r@(NTIMES 0 _) = do
      modifyIORef' count (+1)
      up r m 
    down' m (NTIMES n r) = do
      modifyIORef' count (+1)
      nu <- nullable r
      nr <- createRexp (NTIMES (n-1) r)
      if nu then do
        mBott <- mBottom
        let m' = mBott {
          start = start m
        }
        writeIORef (parents m') [AltC m]
        down (SeqC m' sBottom [] [nr]) r
        up (NTIMES (n-1) r) m'
      else down (SeqC m sBottom [] [nr]) r
    down' m (RECD s r) = do
      modifyIORef' count (+1)
      down (RecdC m s) r
    down' m (RANGE cs)
      | Set.member c cs = do
        modifyIORef' count (+1)
        modifyIORef workList (++ [Zipper (SEQ c []) m]) 
      | otherwise = modifyIORef' count (+1)

    up :: Rexp' -> Mem -> IO ()
    up r' m = do
      r <- rBottom
      writeIORef (rexp' r) r'
      writeIORef (end m) pos
      writeIORef (result m) r
      ps <- readIORef (parents m)
      mapM_ (up' r) ps

    up' :: Rexp -> Context -> IO ()
    up' r TopC = do
      modifyIORef tops (++ [r])
    up' r (SeqC m s rs []) = do
      modifyIORef' count (+1)
      up (SEQ s (reverse (r:rs))) m
    up' r (SeqC m s left (r':right)) = do
      modifyIORef' count (+1)
      nu <- nullable r'
      ws <- readIORef workList
      down (SeqC m s (r:left) right) r'
      ws' <- readIORef workList
      if nu && (ws == ws') then do
        up' r (SeqC m s left right)
      else return ()
    up' r (AltC m) = do
      end' <- readIORef (end m)
      if pos == end' then do
        res <- readIORef (result m)
        r' <- readIORef (rexp' res)
        case r' of
          (ALT _) -> modifyIORef' count (+1)
          -- ^ We only take the first successful match of the alternate.
          _ -> error "Not an ALT "
      else do
        modifyIORef' count (+1)
        up (ALT [r]) m
    up' r (StarC m rs r') = do
      let ct = StarC m (r:rs) r'
      ws' <- readIORef workList 
      down ct r'
      ws <- readIORef workList
      if ws == ws' then do
        modifyIORef' count (+1)
        up (SEQ sBottom (reverse(r:rs))) m
      else return ()
    up' r (OptionalC m) = do
      r' <- readIORef (rexp' r)
      up r' m
      modifyIORef' count (+1)
    -- ^ up' on this case is called on a successful first-time match.
    up' r (RecdC m s) = do
      up (RECD s r) m
      modifyIORef' count (+1)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat Prelude.<$> mapM f xs

{-| Takes a list of Rexp derived from the `run` function
  and returns whether the operation was successful or not.
-}
matcher :: [Char] -> Rexp -> IO Bool
matcher s r = do
  res <- run s r
  return (not $ null res)

unwrapTopRexp :: Rexp -> IO Rexp
unwrapTopRexp r = do
  r' <- readIORef (rexp' r)
  case r' of
    (SEQ _ rs@(_:_)) -> return (last rs)
    _ -> error "Invalid top exp" 

run :: [Char] -> Rexp -> IO [Rexp]
run s r = do
  z <- focus r
  writeIORef workList [z]
  run' 0 s

run' :: Int -> [Char] -> IO [Rexp]
run' pos s = do
  ws <- readIORef workList
  writeIORef workList []
  writeIORef tops []
  case s of
    [] -> do
      mapM_ (der pos cBottom) ws
      writeIORef workList ws
      ts <- readIORef tops
      mapM unwrapTopRexp ts
    (c:cs) -> do
      mapM_ (der pos c) ws
      run' (pos + 1) cs

{-| Flattens an Rexp' into a String.

  Based off of Urban's `flatten` from his method of lexing
  with derivatives.

  The reason why the `flatten` method does not have
  extensive matching is because in all terminal cases of `up'`,
  we return a SEQ, ALT or RECD when going back up the zipper, 
  so there is no need to match any other Rexp
-}
flatten :: Rexp -> IO [Char]
flatten r = do
  r' <- readIORef (rexp' r)
  case r' of
    (SEQ s []) -> do
      if s == '\0' then return [] else return [s]
    (SEQ _ rs) -> concatMapM flatten rs
    (ALT rs) -> flatten (head rs)
    (RECD _ r) -> flatten r
    _ -> return ['\0']

{-| Returns a list of (String, String) pairs that will be used
  for tokenising.

  Based off of Urban's `env` from his method of lexing
  with derivatives.

  Like `flatten`, the reason why `env` does not have
  extensive matching is because in all terminal cases of `up'`,
  we return a SEQ, ALT or RECD when going back up the zipper, 
  so there is no need to match any other Rexp
-}
env :: Rexp -> IO [([Char], [Char])]
env r = do
  r' <- readIORef (rexp' r)
  case r' of
    (SEQ _ []) -> return []
    (SEQ _ rs) -> concatMapM env rs -- | Continue looking for a RECD...
    (ALT rs) -> env (head rs) -- | Continue looking for a RECD...
    (RECD s e) -> do
      v <- flatten e
      vs <- env e
      return ((s, v):vs)
    _ -> return []

lexing :: IO [Rexp] -> IO [[([Char], [Char])]]
lexing rsIO = do
  rs <- rsIO
  mapM env rs

-- | Simplifications - 1 . r == r
isEmptySeq :: Rexp' -> Bool
isEmptySeq (SEQ _ []) = True
isEmptySeq _  = False

simp :: Rexp' -> IO Rexp'
simp (SEQ s rs) = do
  rsFlattened <- flattenSEQ rs
  rs'' <- mapM (readIORef . rexp') rsFlattened
  rs' <- mapM simp rs''

  if ALT [] `elem` rs'
  then return (ALT [])
  else do
    let filteredEs = filter (not . isEmptySeq) rs'
    rsSimp <- mapM createRexp (filter (not . isEmptySeq) filteredEs)    
    return (SEQ s rsSimp)
  where
    flattenSEQ :: [Rexp] -> IO [Rexp]
    flattenSEQ [] = return []
    flattenSEQ (r:rs) = do
      r' <- readIORef (rexp' r)
      case r' of 
        (SEQ s' rs') | s == s' -> do
          rsFlat' <- flattenSEQ rs'
          rsFlat <- flattenSEQ rs
          return (rsFlat' ++ rsFlat)
        _ -> do
          rsFlat <- flattenSEQ rs
          return (r:rsFlat)

simp (ALT rs) = do
  rsFlattened <- flattenALT rs
  rs'' <- mapM (readIORef . rexp') rsFlattened
  rs' <- mapM simp rs''
  let filteredEs = filter (/= ALT []) rs'
  rsSimp <- mapM createRexp (filter (/= ALT []) filteredEs)
  return (ALT rsSimp)
  where
    flattenALT :: [Rexp] -> IO [Rexp]
    flattenALT [] = return []
    flattenALT (r:rs) = do
      r' <- readIORef (rexp' r)
      case r' of 
        (ALT rs') -> do
          rsFlat' <- flattenALT rs'
          rsFlat <- flattenALT rs
          return (rsFlat' ++ rsFlat)
        _ -> do
          rsFlat <- flattenALT rs
          return (r:rsFlat)

simp r' = return r'

-- | String extensions to make creation of Rexps easier.
stringToRexp' :: [Char] -> IO Rexp'
stringToRexp' [] = return (SEQ sBottom [])
stringToRexp' [c] = return (CHAR c)
stringToRexp' (c:cs) = do
  r <- stringToRexp [c]
  rs <- mapM (createRexp . CHAR) cs
  return (SEQ sBottom (r:rs))

stringToRexp :: [Char] -> IO Rexp
stringToRexp s = do
  r' <- stringToRexp' s
  createRexp r'

class ToRexp' a where
  toRexp' :: a -> IO Rexp'

instance ToRexp' (IO Rexp') where
  toRexp' :: IO Rexp' -> IO Rexp'
  toRexp' = id

instance ToRexp' Rexp' where
  toRexp' :: Rexp' -> IO Rexp'
  toRexp' = return

instance ToRexp' Rexp where
  toRexp' :: Rexp -> IO Rexp'
  toRexp' r = readIORef (rexp' r)

instance ToRexp' (IO Rexp) where
  toRexp' :: IO Rexp -> IO Rexp'
  toRexp' rIO = do
    r <- rIO
    readIORef (rexp' r)

instance ToRexp' String where
  toRexp' = stringToRexp'


class ToRexp a where
  toRexp :: a -> IO Rexp

instance ToRexp Rexp where
  toRexp :: Rexp -> IO Rexp
  toRexp = return

instance ToRexp (IO Rexp) where
  toRexp :: IO Rexp -> IO Rexp
  toRexp = id

instance ToRexp (IO Rexp') where
  toRexp :: IO Rexp' -> IO Rexp
  toRexp rIO' = do
    r' <- rIO'
    createRexp r'

instance ToRexp Rexp' where
  toRexp :: Rexp' -> IO Rexp
  toRexp = createRexp

instance ToRexp String where
  toRexp = stringToRexp

infixl 9 ^>
infixl 8 ?>
infixl 7 +>
infixl 6 *>
infixl 4 <~>
infixl 3 <|>
infixl 1 <$>

(<~>) :: (ToRexp a, ToRexp b) => a -> b -> IO Rexp
a <~> b = do
  ae <- toRexp a
  be <- toRexp b
  createRexp (SEQ '\0' [ae,be])

(<|>) :: (ToRexp a, ToRexp b) => a -> b -> IO Rexp
a <|> b = do
  ae <- toRexp a
  be <- toRexp b
  createRexp (ALT [ae, be])

(<$>) :: ToRexp a => String -> a -> IO Rexp
s <$> a = do
  e <- toRexp a
  createRexp (RECD s e)

(*>) :: ToRexp a => a -> b -> IO Rexp
a *> _ = do
  e <- toRexp a
  createRexp (STAR e)

(+>) :: ToRexp a => a -> b -> IO Rexp
a +> _ = do
  e <- toRexp a
  createRexp (PLUS e)

(?>) :: ToRexp a => a -> b -> IO Rexp
a ?> _ = do
  e <- toRexp a
  createRexp (OPTIONAL e)

(^>) :: ToRexp a => a -> Int -> IO Rexp
a ^> n = do
  e <- toRexp a
  createRexp (NTIMES n e)

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

pps :: [Rexp] -> String
pps rs = indent (map ppe rs)

ppe :: Rexp -> String
ppe (Rexp _ r') =
  "EXP\n" ++
  indent ("Mem: { ... }" : [ppe' (unsafePerformIO (readIORef r'))])

ppe' :: Rexp' -> String
ppe' (CHAR c)     = c : "\n"
ppe' (SEQ s rs)  =  (if null rs then "SEQ "++[s] else "SEQ\n" ++ pps rs) ++ "\n"
ppe' (ALT rs) = (if null rs then "ALT []\n" else "ALT\n"++ pps rs) ++ "\n"
ppe' (RANGE cs) = Set.showTreeWith True False cs ++ "\n"
ppe' (STAR r) = "STAR\n" ++ indent [ppe r]
ppe' (PLUS r) = "PLUS\n" ++ indent [ppe r]
ppe' (OPTIONAL r) = "OPTIONAL\n" ++ indent [ppe r]
ppe' (NTIMES n r) = "NTIMES " ++ show n ++ "\n" ++ indent [ppe r]
ppe' (RECD s r) = "RECD " ++ s ++ "\n" ++ indent [ppe r]

ppz :: Zipper -> String
ppz (Zipper r' m) = "ZIP\n" ++ indent (ppe' r':[ppm m])

ppm :: Mem -> String
ppm (Mem st en ps res) =
  "MEM\n" ++
  "Start: " ++ show st ++ "\n" ++
  "End: " ++ show (unsafePerformIO (readIORef en)) ++ "\n" ++
  "Parents:\n" ++
  indent (map ppctx (unsafePerformIO (readIORef ps))) ++ "\n" ++
  "Result:\n" ++
  indent [ppe (unsafePerformIO (readIORef res))]

ppctx :: Context -> String
ppctx TopC = "TOP\n"
ppctx (SeqC m _ _ _) = "SEQC\n" ++ indent [ppm m]
ppctx (AltC m) = "ALTC\n" ++ indent [ppm m]
ppctx (StarC m rs r) = "STARC\n" ++ indent [ppm m]
-- ppctx (PlusC m es e) = "PLUSC\n" ++ indent [ppm m]
ppctx (OptionalC m) = "OPTIONALC\n" ++ indent [ppm m]
-- ppctx (NTimesC m n es e) = "NTIMESC " ++ show n ++ "\n" ++ indent [ppm m]
ppctx (RecdC m s) = "RECDC " ++ s ++ "\n" ++ indent [ppm m]
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
            | StarC Mem [Exp] Exp
            -- | PlusC Mem [Exp] Exp
            | OptionalC Mem
            -- | NTimesC Mem Int [Exp] Exp Bool
            | RecdC Mem [Char]
            deriving (Ord, Eq, Show)

data Zipper = Zipper Exp' Mem deriving (Ord, Show)

instance Eq Zipper where
  (Zipper e m) == (Zipper e' m') =
    e == e' && m == m'

data Mem = Mem
  {
    start :: Int
    , end :: IORef Int
    , parents :: IORef [Context]
    , result :: IORef Exp
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
  end' <- newIORef (-1)
  let e = Exp {
        mem = mRef
        , exp' = e'
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
cBottom :: Char
cBottom = '\0'

{-| Placeholder for the symbol of a SEQ.
-}
sBottom :: Char
sBottom = '\0'

{-| Creates a zipper that focuses on the given
  expression.
-}
focus :: Exp -> IO Zipper
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
nullable :: Exp -> IO Bool
nullable e = do
  e' <- readIORef (exp' e)
  case e' of
    (CHAR _) -> return False
    (ALT es) -> process id False (||) es
    (SEQ _ []) -> return True
    (SEQ _ es) -> process not True (&&) es
    (STAR _) -> return True
    (PLUS e'') -> nullable e''
    (OPTIONAL _) -> return True
    (NTIMES 0 _) -> return True
    (NTIMES _ e'') -> nullable e''
    (RECD _ e'') -> nullable e''
    (RANGE _) -> return False
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
      (e:es) - The list of elements to process.
    -}
    process :: (Bool -> Bool) -> Bool -> (Bool -> Bool -> Bool) -> [Exp] -> IO Bool
    process _ identity _ [] = return identity
    process condition identity combine (e:es) = do
      b <- nullable e
      if condition b
        then return b
        else do
          rest <- process condition identity combine es
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
tops :: IORef [Exp]
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
der pos c (Zipper ex me) = up ex me
    where
    down :: Context -> Exp -> IO ()
    down ct e = do
      m <- readIORef (mem e)
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
        writeIORef (mem e) newMem
        e' <- readIORef (exp' e)
        down' newMem e'

    down' :: Mem -> Exp' -> IO ()
    down' m (CHAR d)
        | c == d = do
          modifyIORef workList (++ [Zipper (SEQ c []) m])
        | otherwise = return ()
    down' m r@(SEQ _  []) = up r m
    down' m (SEQ s (e:es)) = do
      nu <- nullable e
      if nu then do
        mBott <- mBottom
        let m' = mBott {
          start = start m
        }
        writeIORef (parents m') [AltC m]
        down (SeqC m' s [] es) e
        case es of
          [] -> return () -- | Derive as normal...
          (er:esr) -> down (SeqC m' s [] esr) er
          -- ^ Derive as if the first part matched the empty string...
      else down (SeqC m s [] es) e
    down' m (ALT es) = mapM_ (down (AltC m)) es
    down' m (STAR e) = down (StarC m [] e) e
    down' m (PLUS e) = down (StarC m [] e) e
    down' m (OPTIONAL e) = down (OptionalC m) e
    down' m r@(NTIMES 0 _) = up r m 
    down' m (NTIMES n e) = do
      nu <- nullable e
      ne <- createExp (NTIMES (n-1) e)
      if nu then do
        mBott <- mBottom
        let m' = mBott {
          start = start m
        }
        writeIORef (parents m') [AltC m]
        down (SeqC m' sBottom [] [ne]) e
        up (NTIMES (n-1) e) m'
      else down (SeqC m sBottom [] [ne]) e
    down' m (RECD s e) = down (RecdC m s) e
    down' m (RANGE cs)
      | Set.member c cs = do
          modifyIORef workList (++ [Zipper (SEQ c []) m]) 
      | otherwise = return ()

    up :: Exp' -> Mem -> IO ()
    up e' m = do
      e <- eBottom
      writeIORef (exp' e) e'
      writeIORef (end m) pos
      writeIORef (result m) e
      ps <- readIORef (parents m)
      mapM_ (up' e) ps

    up' :: Exp -> Context -> IO ()
    up' e TopC = do
      modifyIORef tops (++ [e])
    up' e (SeqC m s es []) = up (SEQ s (reverse (e:es))) m
    up' e (SeqC m s el (er:esr)) = do
      nu <- nullable er
      ws <- readIORef workList
      down (SeqC m s (e:el) esr) er
      ws' <- readIORef workList
      if nu && (ws == ws') then do
        up' e (SeqC m s el esr)
      else return ()
    up' e (AltC m) = do
      end' <- readIORef (end m)
      if pos == end' then do
        res <- readIORef (result m)
        e' <- readIORef (exp' res)
        case e' of
          (ALT _) -> return () 
          -- ^ We only take the first successful match of the alternate.
          _ -> error "Not an ALT "
      else up (ALT [e]) m
    up' e (StarC m es e') = do
      let ct = StarC m (e:es) e'
      ws' <- readIORef workList 
      down ct e'
      ws <- readIORef workList
      if ws == ws' then do
        up (SEQ sBottom (reverse(e:es))) m
      else return ()
    up' e (OptionalC m) = do
      e' <- readIORef (exp' e)
      up e' m
    -- ^ up' on this case is called on a successful first-time match.
    up' e (RecdC m s) = up (RECD s e) m
    up' _ _ = return ()

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat Prelude.<$> mapM f xs

{-| Takes a list of Exp derived from the `run` function
  and returns whether the operation was successful or not.
-}
matcher :: [Char] -> Exp -> IO Bool
matcher s r = do
  res <- run s r
  return (not $ null res)

unwrapTopExp :: Exp -> IO Exp
unwrapTopExp e = do
  e' <- readIORef (exp' e)
  case e' of
    (SEQ _ es@(_:_)) -> return (last es)
    _ -> error "Invalid top exp" 

run :: [Char] -> Exp -> IO [Exp]
run s e = do
  z <- focus e
  writeIORef workList [z]
  run' 0 s

run' :: Int -> [Char] -> IO [Exp]
run' pos s = do
  ws <- readIORef workList
  writeIORef workList []
  writeIORef tops []
  case s of
    [] -> do
      mapM_ (der pos cBottom) ws
      writeIORef workList ws
      ts <- readIORef tops
      mapM unwrapTopExp ts
    (c:cs) -> do
      mapM_ (der pos c) ws
      run' (pos + 1) cs

{-| Flattens an Exp' into a String.

  Based off of Urban's `flatten` from his method of lexing
  with derivatives.

  The reason why the `flatten` method does not have
  extensive matching is because in all terminal cases of `up'`,
  we return a SEQ, ALT or RECD when going back up the zipper, 
  so there is no need to match any other Exp
-}
flatten :: Exp -> IO [Char]
flatten e = do
  e' <- readIORef (exp' e)
  case e' of
    (SEQ s []) -> do
      if s == '\0' then return [] else return [s]
    (SEQ _ es) -> concatMapM flatten es
    (ALT es) -> flatten (head es)
    (RECD _ e) -> flatten e
    _ -> return ['\0']

{-| Returns a list of (String, String) pairs that will be used
  for tokenising.

  Based off of Urban's `env` from his method of lexing
  with derivatives.

  Like `flatten`, the reason why `env` does not have
  extensive matching is because in all terminal cases of `up'`,
  we return a SEQ, ALT or RECD when going back up the zipper, 
  so there is no need to match any other Exp
-}
env :: Exp -> IO [([Char], [Char])]
env e = do
  e' <- readIORef (exp' e)
  case e' of
    (SEQ _ []) -> return []
    (SEQ _ es) -> concatMapM env es -- | Continue looking for a RECD...
    (ALT es) -> env (head es) -- | Continue looking for a RECD...
    (RECD s e) -> do
      v <- flatten e
      vs <- env e
      return ((s, v):vs)
    _ -> return []

lexing :: IO [Exp] -> IO [[([Char], [Char])]]
lexing esIO = do
  es <- esIO
  mapM env es

-- | Simplifications - 1 . r == r
isEmptySeq :: Exp' -> Bool
isEmptySeq (SEQ _ []) = True
isEmptySeq _  = False

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
stringToExp' [] = return (SEQ sBottom [])
stringToExp' [c] = return (CHAR c)
stringToExp' (c:cs) = do
  e <- stringToExp [c]
  es <- mapM (createExp . CHAR) cs
  return (SEQ sBottom (e:es))

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
  "End: " ++ show (unsafePerformIO (readIORef en)) ++ "\n" ++
  "Parents:\n" ++
  indent (map ppctx (unsafePerformIO (readIORef ps))) ++ "\n" ++
  "Result:\n" ++
  indent [ppe (unsafePerformIO (readIORef res))]

ppctx :: Context -> String
ppctx TopC = "TOP\n"
ppctx (SeqC m _ _ _) = "SEQC\n" ++ indent [ppm m]
ppctx (AltC m) = "ALTC\n" ++ indent [ppm m]
ppctx (StarC m es e) = "STARC\n" ++ indent [ppm m]
-- ppctx (PlusC m es e) = "PLUSC\n" ++ indent [ppm m]
ppctx (OptionalC m) = "OPTIONALC\n" ++ indent [ppm m]
-- ppctx (NTimesC m n es e) = "NTIMESC " ++ show n ++ "\n" ++ indent [ppm m]
ppctx (RecdC m s) = "RECDC " ++ s ++ "\n" ++ indent [ppm m]
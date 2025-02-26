module RexpZipperv2 where

import Data.Map.Strict as M
import GHC.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Monad (foldM)
import Data.Ord (comparing)

type Sym = Char

data Exp = ZERO
            | ONE
            | CHAR Char
            | SEQ Sym [Exp]
            | ALT [Exp]
            deriving (Ord, Eq, Show)

-- instance Show Exp where
--   show (ALT es) = show (unsafePerformIO (readIORef es))
--   show r = show r

data Context = TopC
            | SeqC Mem Sym [Exp] [Exp] -- Sequence that has its own context, the symbol it represented, left siblings (processed), right siblings (unprocessed)
            | AltC Mem -- Alternate has one context shared between its children
            deriving (Ord, Eq, Show)

data Zipper = Zipper Exp Mem deriving (Ord, Eq, Show)

data Mem = Mem
  { parents :: IORef [Context]
  , result :: IORef (M.Map Int Exp)
  }

instance Eq Mem where
  (Mem _ r1) == (Mem _ r2) = r1 == r2

instance Ord Mem where
  compare = comparing (unsafePerformIO . readIORef . result)

instance Show Mem where
  show (Mem ps r) =
    "Mem { parents = " ++ 
    show (unsafePerformIO (readIORef ps)) ++ 
    "\nresult = " ++ 
    show (unsafePerformIO (readIORef r)) ++ 
    "}"

type Mems = M.Map (Int, Exp) Mem

mems :: IORef Mems
{-# NOINLINE mems #-}
mems = unsafePerformIO (newIORef M.empty)

resetMems :: IORef Mems -> IO ()
resetMems ms = writeIORef ms M.empty

lookupMem :: (Int, Exp) -> IO (Maybe Mem)
lookupMem key = do
  memsMap <- readIORef mems
  return $ M.lookup key memsMap

lookupMemsSafe :: (Int, Exp) -> IO Mem -> IO Mem
lookupMemsSafe key defaultMemIO = do
  maybeMem <- lookupMem key
  maybe defaultMemIO return maybeMem

defaultMem :: Mem
defaultMem = Mem
  { parents = unsafePerformIO (newIORef [TopC])
  , result = unsafePerformIO (newIORef M.empty)
  }

focus :: Exp -> Zipper
focus r = Zipper r mem
  where 
    mem :: Mem
    mem = defaultMem { 
      parents = unsafePerformIO (newIORef [SeqC defaultMem '\0' [] [r]]) 
    }

nullable :: Exp -> Bool
nullable ZERO = False
nullable ONE = True
nullable (CHAR _) = False
nullable (ALT es) = any nullable es
nullable (SEQ _ es) = all nullable es

-- isNullable :: Context -> Bool
-- isNullable TopC = False
-- isNullable (AltC ct) = isNullable ct
-- isNullable (SeqC TopC _ _ _) = True
-- isNullable (SeqC ct _ _ es) = Prelude.null es && isNullable ct

der ::  Int -> Char -> Zipper -> IO [Zipper]
der pos c (Zipper re mem) = up re mem
    where
    down :: Context -> Exp -> IO [Zipper]
    down ct e = do
      globalMems <- readIORef mems
      let key = (pos, e)
      case M.lookup key globalMems of
        Just m -> do
          _ <- modifyIORef' (parents m) (++ [ct])
          results <- readIORef (result m)
          case M.lookup pos results of
            Just e' -> up' e' ct
            Nothing -> return []
        Nothing -> do
          newParents <- newIORef [ct]
          let newMem = defaultMem { parents = newParents }
          modifyIORef' mems (M.insert key newMem)
          down' newMem e

    down' :: Mem -> Exp -> IO [Zipper]
    down' _ ZERO = return []
    down' _ ONE = return []
    down' m (CHAR d)
        | c == d = return [Zipper (SEQ c []) m]
        | otherwise = return []
    down' m r@(SEQ _ []) = up r m
    down' m (SEQ s (e:es)) = do
      newParent <- newIORef [AltC m]
      let m' = defaultMem { parents = newParent } 
      down (SeqC m' s [] es) e
    down' m (ALT es) =
      -- exps <- readIORef es
      foldM (\acc e -> do
        zs <- down (AltC m) e
        return (acc ++ zs)
        ) [] es

    up :: Exp -> Mem -> IO [Zipper]
    up e m = do
      _ <- modifyIORef' (result m) (M.insert pos e )
      ps <- readIORef (parents m)
      parentResults <- mapM (up' e) ps
      return $ concat parentResults

    up' :: Exp -> Context -> IO [Zipper]
    up' _ TopC = return []
    up' e (SeqC m s es []) = up (SEQ s (reverse (e:es))) m
    up' e (SeqC m s el (er:esr)) = down (SeqC m s (e:el) esr) er
    up' e (AltC m) = do
      results <- readIORef (result m)
      case M.lookup pos results of 
        Just (ALT es) -> do
          _ <- modifyIORef' (result m) (M.insert pos (ALT (es ++ [e])))
          return []
        _ -> do
          up (ALT [e]) m

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

ders :: Int -> [Char] -> [Zipper] -> IO [Zipper]
ders (-1) s zs = do
  _ <- resetMems mems
  ders 0 s zs
ders _ [] zs = return $ concatMap (\z@(Zipper r _) -> [z | nullable r]) zs
ders pos (c:cs) zs = do
    newZs <- concatMapM (der pos c) zs
    ders (pos + 1) cs newZs

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
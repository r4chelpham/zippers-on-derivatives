{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module EdelmannZipper where

import Rexp
import qualified Data.Set as Set
import Data.Function.Memoize
import Data.Char (intToDigit)
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (listToMaybe, isJust)
import Text.Printf (printf)
import Lexer(Token(..), keyword, op, string, parens, semi, whitespace, identifier, numbers, comment)

type Context = [Rexp]
type Zipper = Set.Set Context

instance Memoizable Zipper where
  memoize f s = memoize (\cs -> f (Set.fromList cs)) (Set.toAscList s)

focus :: Rexp -> Zipper
focus r = Set.singleton [r]

up :: Char -> Context -> Zipper
up _ [] = Set.empty
up c (right:parent)
    | nullable right = Set.union (down right c parent) (up c parent)
    | otherwise = down right c parent

down :: Rexp -> Char -> Context -> Zipper
down ZERO _ _ = Set.empty
down ONE _ _ = Set.empty
down (CHAR d) c ctx
    | c == d = Set.singleton ctx
    | otherwise = Set.empty
down (ALT r1 r2) c ctx =
    Set.union (down r1 c ctx) (down r2 c ctx)
down (SEQ r1 r2) c ctx
    | nullable r1 =
        Set.union (down r1 c (r2:ctx)) (down r2 c ctx)
    | otherwise = down r1 c (r2:ctx)
down r@(STAR r1) c ctx = down r1 c (r:ctx)
down (OPTIONAL r1) c ctx = down r1 c ctx
down (RANGE cs) c ctx
    | Set.member c cs = Set.singleton ctx
    | otherwise = Set.empty
down (PLUS r1) c ctx = down r1 c (STAR r1:ctx)
down (NTIMES _ 0) _ _ = Set.empty
down (NTIMES r1 n) c ctx = down r1 c (NTIMES r1 (n-1):ctx)
down (RECD _ r1) c ctx = down r1 c ctx

der :: Zipper -> Char -> Zipper
der z c = Set.unions $ Set.map (up c) z

ders :: Zipper -> [Char] -> Zipper
ders z [] = z
ders z (c:cs) = EdelmannZipper.ders (EdelmannZipper.der z c) cs

isNullable :: Zipper -> Bool
isNullable = any nullableCtx
    where
    nullableCtx :: Context -> Bool
    nullableCtx [] = True
    nullableCtx ctx = all nullable ctx

matcher :: Rexp -> [Char] -> Bool
matcher r s = isNullable (EdelmannZipper.ders (focus r) s)

hasFirst :: Zipper -> Bool
hasFirst z = any (\c -> any Rexp.hasFirst c && all isProductive c) z

type Action t = ActionContext t -> IO ()

data ActionContext t = ActionContext {
    ctxemit :: t -> IO ()
    , ctxcontent :: String
}

data Rule t = Rule {
    r :: Rexp
    , action :: Action t
}

data State t = State {
    next :: Char -> State t
    , hasNext :: Bool
    , value :: Maybe (Action t)
}

type Transitions t = IORef (Map.Map Char (State t))

instance Show (Action t) where
    show _ = "Action"

instance Show (State t) where
    show State{..} = concat
        [
            "State { ",
            "hasNext = ", show hasNext,
            ", hasValue = ", show (Data.Maybe.isJust value),
            " }"
        ]

instance Show (Rule t) where
    show Rule{..} = concat
        [
            "Rule { ",
            "regex = ", show r,
            " }"
        ]

{- Debug wrappers for memoisation -}
debugState :: State t -> IO ()
debugState state = do
    printf "State: %s\n" (show state)

debugTransition :: State t -> Char -> IO (State t)
debugTransition state c = do
    let nextState = next state c
    printf "Transition '%c': %s -> %s\n" c (show state) (show nextState)
    return nextState

debugAction :: Action t -> ActionContext t -> IO ()
debugAction action ctx = do
    printf "Executing Action with context: %s\n" (show $ ctxcontent ctx)
    action ctx

{- Build an automata (memoisation) from the expressions of each rule -}
build :: forall a. [Rule a] -> State a
build rules = getState (map (\(Rule rexp _) -> focus rexp) rules)
    where
        getState = memoize buildState

        buildState :: [Zipper] -> State a
        buildState zs = unsafePerformIO $ do
            transitions <- newIORef Map.empty
            return $ State
                {
                    next = \c -> unsafePerformIO $ getNextState transitions zs c
                    , hasNext = any EdelmannZipper.hasFirst zs
                    , value = computeValue zs rules
                }

        getNextState :: Transitions a -> [Zipper] -> Char -> IO (State a)
        getNextState transitions zs c = do
            cache <- readIORef transitions
            case Map.lookup c cache of
                Just state -> return state
                Nothing -> do
                    let nextZs = map (`EdelmannZipper.der` c) zs
                        next = getState nextZs
                    modifyIORef transitions (Map.insert c next)
                    return next

computeValue :: [Zipper] -> [Rule a] -> Maybe (Action a)
computeValue zs rs =
    listToMaybe
    [
        action | (z, Rule{..}) <- zip zs rs
        , isNullable z
    ]

tokenise :: State a -> [Char] -> IO [a]
tokenise init input = do
    tokens <- newIORef []
    let emit t = modifyIORef tokens (++ [t])
        process :: [Char] -> IO ()
        process [] = return ()
        process s@(_:cs) = do
            (bestAction, bestLen) <- findBestAction init s 0 Nothing 0
            case bestAction of
                Just action -> do
                    let (tokenised, remaining) = splitAt bestLen s
                    let ctx = ActionContext { ctxemit = emit, ctxcontent = tokenised }
                    action ctx
                    process remaining
                Nothing -> process cs

    process input
    readIORef tokens

findBestAction :: State a -> [Char] -> Int -> Maybe (Action a) -> Int -> IO (Maybe (Action a), Int)
findBestAction _ [] _ bestAction bestLen = return (bestAction, bestLen)
findBestAction state (c:cs) currentLen bestAction bestLen
    | not (hasNext state) = return (bestAction, bestLen)
    | otherwise = do
        let newState = next state c
            newLen = currentLen + 1
            newBest = case value newState of
                Nothing -> (bestAction, bestLen)
                action@(Just _) -> (action, newLen)
        findBestAction newState cs newLen (fst newBest) (snd newBest)

emit :: t -> ActionContext t -> IO ()
emit token ctx = ctxemit ctx token

content :: ActionContext t -> String
content = ctxcontent

{- Rules for the WHILE language -}
rules :: [Rule Token]
rules = 
    [ Rule keyword (\ctx -> emit (T_KEYWORD (content ctx)) ctx)
    , Rule op      (\ctx -> emit (T_OP (content ctx)) ctx)
    , Rule string  (\ctx -> emit (T_STRING (content ctx)) ctx)
    , Rule parens  (\ctx -> emit (T_PAREN (content ctx)) ctx)
    , Rule semi    (emit T_SEMI)
    , Rule whitespace (\_ -> return ())
    , Rule identifier (\ctx -> emit (T_ID (content ctx)) ctx)
    , Rule numbers (\ctx -> emit (T_NUM (content ctx)) ctx)
    , Rule comment (\_ -> return ())
    ]

-- pretty-printing Zippers
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
pps es = indent (map pp es)

pp :: Rexp -> String
pp ZERO        = "0\n"
pp ONE         = "1\n"
pp (CHAR c)     = c : "\n"
pp (RANGE cs)     = Set.showTreeWith True False cs ++ "\n"
pp (SEQ r1 r2)  =  "SEQ\n" ++ pps [r1, r2]
pp (ALT r1 r2) = "ALT\n" ++ pps [r1, r2]
pp (STAR r)    = "STAR\n" ++ pps [r]
pp (OPTIONAL r)    = "OPTIONAL\n" ++ pps [r]
pp (PLUS r)    = "PLUS\n" ++ pps [r]
pp (NTIMES r n)    = "NTIMES\n" ++ [intToDigit n] ++ "\n" ++ pps [r]
pp (RECD s r)    = "RECD\n" ++ s ++ "\n" ++ pps [r]

ppz :: Zipper -> String
ppz z = "ZIP\n" ++ indent (map ppctx (Set.toList z))

ppctx :: Context -> String
ppctx ct = indent (map pp ct)

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EdelmannLexer where 

import Data.IORef
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (listToMaybe, isJust)
import Text.Printf (printf)
import Rexp
import EdelmannZipper
import Token
import WhileRegisters
import Data.Function.Memoize

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

lexing :: State a -> [Char] -> IO [a]
lexing qinit input = do
    tokens <- newIORef []
    let emit t = modifyIORef tokens (++ [t])
        process :: [Char] -> IO ()
        process [] = return ()
        process s@(_:cs) = do
            (bestAction, bestLen) <- findBestAction qinit s 0 Nothing 0
            case bestAction of
                Just action -> do
                    let (tokenised, remaining) = splitAt bestLen s
                    let ctx = ActionContext { ctxemit = emit, ctxcontent = tokenised }
                    action ctx
                    process remaining
                Nothing -> process cs

    process input
    readIORef tokens

{- Find the best (longest match and highest priority) action -}
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

{- Override emit and content for WHILE Language tokens -}
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

tokenise :: [Char] -> IO [Token]
tokenise s = do
    let states = build rules
    lexing states s
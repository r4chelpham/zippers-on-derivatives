-- TODO: test the damn code!!!!!!

main :: IO ()
main = putStrLn "Hello, Haskell"

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)
{- context consists of a direction, value, tree (traverse down) 
and context (traverse up) -}
data Context a = T
                | L a (Tree a) (Context a)
                | R a (Tree a) (Context a) deriving (Show)
data Zipper a = Zipper (Tree a) (Context a) deriving (Show)

left :: Zipper a -> Zipper a
left (Zipper (Node x l r) c) = Zipper l (R x r c) -- when you go back up the tree you can only go right
left (Zipper Nil c) = Zipper Nil c

right :: Zipper a -> Zipper a
right (Zipper (Node x l r) c) = Zipper r (L x l c) -- when you go back up the tree you can only go left
right z@(Zipper Nil c) = z

{- 
    TODO: ask if this is correct? you create a "new" tree every 
    time you either go left or right?? 
    that seems a little funky 
-}
up :: Zipper a -> Zipper a
up z@(Zipper _ T) = z -- cannot go back up again if you're at the top
up (Zipper t (L x l c)) = Zipper (Node x l Nil) c
up (Zipper t (R x r c)) = Zipper (Node x Nil r) c

append :: Tree a -> Zipper a -> Zipper a
append t (Zipper _ c) = Zipper t c

-- go all the way back up to the root of the tree
root :: Zipper a -> Zipper a
root z@(Zipper _ T) = z
root z@(Zipper _ _) = up z

-- TODO: decide whether i want to modify the tree?
-- TODO: ask whether it is better to keep the tree you came from traversable when going up?

-- function to make traversing the tree more readable
infixl 1 -:  -- Left-associative, binds loosely so you can chain movements in the zipper L->R
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Nil Nil)
                (Node 'T' Nil Nil)
            )
            (Node 'Y'
                (Node 'S' Nil Nil)
                (Node 'A' Nil Nil)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Nil Nil)
                (Node 'R' Nil Nil)
            )
            (Node 'A'
                (Node 'A' Nil Nil)
                (Node 'C' Nil Nil)
            )
        )

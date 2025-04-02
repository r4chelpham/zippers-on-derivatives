module TreeZipper (
    Tree(..),
    Context(..),
    Zipper(..),
    left,
    right,
    up,
    append,
    root,
    (-:),
) where

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
right z@(Zipper Nil _) = z

{- 
    TODO: ask if this is correct? you create a "new" tree every 
    time you either go left or right?? 
    that seems a little funky 
-}
up :: Zipper a -> Zipper a
up z@(Zipper _ T) = z -- cannot go back up again if you're at the top
up (Zipper _ (L x l c)) = Zipper (Node x l Nil) c
up (Zipper _ (R x r c)) = Zipper (Node x Nil r) c

append :: Tree a -> Zipper a -> Zipper a
append t (Zipper _ c) = Zipper t c

-- go all the way back up to the root of the tree
root :: Zipper a -> Zipper a
root z@(Zipper _ T) = z
root z@(Zipper _ _) = up z

-- function to make traversing the tree more readable
infixl 1 -:  -- Left-associative, binds loosely so you can chain movements in the zipper L->R
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

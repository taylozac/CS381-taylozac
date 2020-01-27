module HW2 where

import Data.Maybe

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))


-- | Encode a list as a tree with only right branches.
--
--   >>> encodeList []
--   End
--
--   >>> encodeList [1,2,3,4]
--   Node 1 End (Node 2 End (Node 3 End (Node 4 End End)))
--
--   >>> encodeList ":-D"
--   Node ':' End (Node '-' End (Node 'D' End End))
--
--Var names:
--  c == current node's value
--  t == tail of the list to encode
--
encodeList :: [a] -> Tree a
encodeList []    = End
encodeList (c:t) = Node c End (encodeList t)


-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--   
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
--Var names:
--  f == the function (a -> b) to be mapped over tree
--  c == current node's value
--  l == left subtree of the current node
--  r == right subtree of the current node
--
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ End          = End
mapTree f (Node c l r) = Node (f c) (mapTree f l) (mapTree f r)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
--Var names:
--  c == current node's value
--  l == left subtree of the current node
--  r == right subtree of the current node
--  t == tail of the path
--
valueAt :: Path -> Tree a -> Maybe a
valueAt _ End              = Nothing
valueAt [] (Node c l r)    = Just c
valueAt (L:t) (Node c l r) = valueAt t l
valueAt (R:t) (Node c l r) = valueAt t r


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
--Var names:
--  v == value to find pathTo
--  c == current node's value
--  l == left subtree of the current node
--  r == right subtree of the current node
--
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo _ End = Nothing
pathTo v (Node c l r)
    | v == c                = Just []
    | pathTo v l /= Nothing = Just ([L] ++ (fromJust (pathTo v l)))
    | pathTo v r /= Nothing = Just ([R] ++ (fromJust (pathTo v r)))
    | otherwise             = Nothing 

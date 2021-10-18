{- Functions on binary trees. Author: Emma Norling Date: Sept 2019 -}
module Tree where
  -- The data type makes no assumptions about it being a BST; it's just a binary tree
  data Tree a = Empty | Node (Tree a) a (Tree a)

  -- declare Tree a to be an instance of Show so we can display it nicely
  instance (Show a) => Show (Tree a) where
    -- will start by a '<' before the root
    -- and put a : a begining of line
    show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
      where
      -- treeshow pref Tree
      --   shows a tree and starts each line with pref
      -- We don't display the Empty tree
      treeshow pref Empty = ""
      -- Leaf
      treeshow pref (Node Empty x Empty) =
                    (pshow pref x)

      -- Right branch is empty
      treeshow pref (Node left x Empty) =
                    (pshow pref x) ++ "\n" ++
                    (showSon pref "`-." "   " left)

      -- Left branch is empty
      treeshow pref (Node Empty x right) =
                    (pshow pref x) ++ "\n" ++
                    (showSon pref "`-." "   " right)

      -- Tree with left and right children non empty
      treeshow pref (Node left x right) =
                    (pshow pref x) ++ "\n" ++
                    (showSon pref "|-." "|  " left) ++ "\n" ++
                    (showSon pref "`-." "   " right)

      -- shows a tree using some prefixes to make it nice
      showSon pref before next t =
                    pref ++ before ++ treeshow (pref ++ next) t

      -- pshow replaces "\n" by "\n"++pref
      pshow pref x = replace '\n' ("\n"++pref) (show x)

      -- replaces one char by another string
      replace c new string =
        concatMap (change c new) string
        where
            change c new x
                | x == c = new
                | otherwise = x:[] -- "x"

  -- Calculate the height (depth) of a Tree
  height :: (Tree a) -> Int
  height Empty = 0
  height (Node left a right)
       = 1 + max (height left) (height right)

  -- Check if a tree is complete (all leaf nodes are at the same level)
  isComplete :: (Tree a) -> Bool
  isComplete Empty = True
  isComplete (Node left a right)
            = isComplete left &&
              isComplete right &&
              height left == height right

  -- Convert a list into a balanced tree (not a BST)
  balance :: [a] -> Tree a
  balance [] = Empty
  balance (x:xs) = Node (balance fstHalf) x (balance sndHalf)
        where (fstHalf, sndHalf) = splitAt ((length xs) `div` 2) xs

  {- Moving on to binary search trees -}

  -- Insert an item into a BST (maintaining a BST)
  insert :: Ord a => a -> Tree a -> Tree a
  insert a Empty = Node Empty a Empty
  insert a (Node left root right)
      | a < root  = Node (insert a left) root right
      | otherwise = Node left root (insert a right)

  -- Convert a list into a BST (using foldr)
  foldTree :: Ord a => [a] -> Tree a
  foldTree = foldr insert Empty

  -- Check if a BST contains a particular item
  contains :: Ord a => a -> Tree a -> Bool
  contains a Empty = False
  contains a (Node left root right)
      | a == root = True
      | a < root  = contains a left
      | otherwise = contains a right

  -- Find the minimum item in a BST
  minimum' :: Eq a => Tree a -> a
  minimum' Empty = error "Empty tree"
  minimum' (Node Empty root _) = root
  minimum' (Node left root _) = minimum' left

  -- Find the maximum item in a BST
  maximum' :: Eq a => Tree a -> a
  maximum' Empty = error "Empty tree"
  maximum' (Node _ root Empty) = root
  maximum' (Node _ root right) = maximum' right

  -- Find the next largest item (to input value) in a BST
  successor :: Ord a => a -> Tree a -> a
  successor val (Node left root right)
      | val == root = minimum' right
      | val < root  = if val >= (maximum' left) then root else successor val left
      | otherwise = successor val right

  {- And now AVL trees (balanced binary search trees -}

  -- Insert an item into an AVL tree
  avlInsert:: Ord a => a -> Tree a -> Tree a
  avlInsert v Empty = Node Empty v Empty
  avlInsert v t@(Node left root right)
      | v > root  = rotate (Node left root (avlInsert v right))
      | v < root  = rotate (Node (avlInsert v left) root right)
      | otherwise = t

  -- Convert a list into an AVL tree
  foldAVLTree :: Ord a => [a] -> Tree a
  foldAVLTree = foldr avlInsert Empty

  -- Check if a binary tree is balanced
  balanced :: (Ord a) => Tree a -> Bool
  balanced Empty = True
  balanced  (Node l root r)
     | not (balanced l) = False
     | not (balanced r) = False
     | abs ((height l) - (height r)) > 1 = False
     | otherwise = True

  -- Rotate a BST, maintaining BST properties, to make it balanced
  rotate :: (Ord a) => Tree a -> Tree a
  rotate Empty = Empty
  rotate (Node l root r)
      | not (balanced l)
                  = Node (rotate l) root r
      | not (balanced r)
                  = Node l root (rotate r)
      | (height l) + 1 < (height r) && (height (left r)) < (height (right r))
                  = Node (Node l root (left r)) (value r) ((right r))
      | (height r) + 1 < (height l) && (height (right l)) < (height (left l))
                  = Node ((left l)) (value l) (Node ((right l)) root r)
      | (height l) + 1 < (height r) && (height (left r)) > (height (right r))
                  = Node (Node l root (left (left r))) (value l)
                    (Node (right (left r)) (value r) (right r))
      | (height r) + 1 < (height l) && (height (right l)) > (height (left l))
                  = Node (Node (left l) (value l) (left (right l))) (value r)
                    (Node (right (right l)) root r)
      | otherwise = Node l root r
      where
      left (Node l root r) = l
      right (Node l root r) = r
      value (Node l root r) = root

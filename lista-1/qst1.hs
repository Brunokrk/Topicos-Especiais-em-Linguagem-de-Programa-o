data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

ins e Leaf = Node e Leaf Leaf
ins e n@(Node x l r)
    | e == x = n
    | e < x = Node x (ins e l) r
    | e > x = Node x l (ins e r)


    
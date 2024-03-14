--Nome: Bruno Marchi Pires
--Questão 1
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving Show

ins e Leaf = Node e Leaf Leaf
ins e n@(Node x l r)
    | e == x = n --retorna a arvore atual (Duplicates shall not pass), Também existem abordagens que permitem valores repetidos, nesse caso seria feito com a utilzação de um contador contando as ocorrências :)
    | e < x = Node x (ins e l) r
    | e > x = Node x l (ins e r)

remove :: (Ord a) => a -> Tree a -> Tree a
remove _ Leaf = Leaf  --elemento não encontrado
remove e (Node x l r)
  | e < x = Node x (remove e l) r  --subárvore esquerda
  | e > x = Node x l (remove e r)  --ubárvore direita
  | otherwise = removeNode (Node x l r)  --Nó encontrado

removeNode :: (Ord a) => Tree a -> Tree a
removeNode (Node _ Leaf Leaf) = Leaf  --é uma folha
removeNode (Node _ l Leaf) = l  -- somente filho a esquerda
removeNode (Node _ Leaf r) = r  -- somente filho a direita
removeNode (Node _ l r) =  -- tem dois filhos... F
  let (minNode, newRight) = extractMin r
  in Node minNode l newRight

extractMin :: (Ord a) => Tree a -> (a, Tree a)
extractMin (Node x Leaf r) = (x, r)
extractMin (Node x l r) =
  let (minNode, newLeft) = extractMin l
  in (minNode, Node x newLeft r)


--Printando estrutura Final para teste
printTree :: (Show a) => Tree a -> IO ()
printTree tree = putStr (treeToString 0 tree)
  where
    treeToString :: (Show a) => Int -> Tree a -> String
    treeToString _ Leaf = ""
    treeToString depth (Node x left right) =
      treeToString (depth + 1) right ++
      replicate (depth * 4) ' ' ++ show x ++ "\n" ++
      treeToString (depth + 1) left

main :: IO ()
main = do
    let tree = foldr ins Leaf [8, 6, 4, 2, 7, 3, 5]
    putStrLn "Árvore original:"
    --print tree
    printTree tree
    let treeAfterRemoval = remove 3 tree
    putStrLn "Árvore após a remoção do elemento 3:"
    --print treeAfterRemoval
    printTree treeAfterRemoval
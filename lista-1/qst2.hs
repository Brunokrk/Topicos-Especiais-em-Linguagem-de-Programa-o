--Nome: Bruno Marchi Pires

--I.Todo nó e vermelho ou preto.
--II.As folhas são pretas.
--III.Se um nó e vermelho seus filhos devem ser pretos.
--IV.Todos os caminhos partindo da raiz até as folhas contém o mesmo número de nós pretos.
--V.Os novos nós devem ser criados como vermelhos.

data RB = R | B deriving Show
data RBTree a = LeafRB | Node a RB (RBTree a) (RBTree a) deriving Show

-- Insere um novo valor na árvore e ajusta a árvore para manter as propriedades do Red-Black Tree.
insertRB :: (Ord a) => a -> RBTree a -> RBTree a
insertRB x s = makeBlack (ins s) -- insere um valor x na árvore s com função auxiliar (ins s)
  where
    -- escopo
    -- raiz  preta
    makeBlack (Node y R a b) = Node y B a b
    makeBlack s = s

    -- Função de inserção 
    ins LeafRB = Node x R LeafRB LeafRB  --vazia
    ins (Node y c a b) --nao vazia
      | x < y     = balance y c (ins a) b
      | x > y     = balance y c a (ins b)
      | otherwise = Node y c a b

    -- Rebalanceamento da árvore - Casos
    balance :: a -> RB -> RBTree a -> RBTree a -> RBTree a
    balance z B (Node y R (Node x R a b) c) d = Node y R (Node x B a b) (Node z B c d)
    balance z B (Node x R a (Node y R b c)) d = Node y R (Node x B a b) (Node z B c d)
    balance z B a (Node x R b (Node y R c d)) = Node y R (Node z B a b) (Node x B c d)
    balance z B a (Node y R (Node x R b c) d) = Node y R (Node z B a b) (Node x B c d)
    balance x c a b = Node x c a b  -- balanceamento inutil

--Tentando printar de forma mais legível para entender se funcionou
printRBTree :: (Show a) => RBTree a -> IO ()
printRBTree tree = putStr (treeToString 0 tree)
  where
    treeToString :: (Show a) => Int -> RBTree a -> String
    treeToString indent LeafRB = replicate (indent * 4) ' ' ++ "Leaf\n"
    treeToString indent (Node x color left right) =
        replicate (indent * 4) ' ' ++ show x ++ " (" ++ show color ++ ")\n" ++
        treeToString (indent + 1) left ++
        treeToString (indent + 1) right

main :: IO ()
main = do
    let emptyTree = LeafRB
        valuesToInsert = [3, 5, 10] 
        populatedTree = foldr insertRB emptyTree valuesToInsert  

    printRBTree populatedTree
    print(populatedTree)

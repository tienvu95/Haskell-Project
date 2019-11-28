-------------------------------------------------------------------------
--
--         CodeTable.hs
--
--         Converting a Huffman tree to a table.
--
-------------------------------------------------------------------------
module CodeTable
  ( codeTable
  ) where
import Types (Tree(Leaf, Node), Bit(L, R), HCode, Table)
import Data.Map (fromList, assocs)



-- Making a table from a Huffman tree.
codeTable :: Ord a => Tree a -> Table a
codeTable t = fromList (convert [] t)

-- Auxiliary function used in conversion to a table. The first argument is
-- the HCode which codes the path in the tree to the current Node, and so
-- codeTable is initialised with an empty such sequence.
convert :: Ord a => HCode -> Tree a -> [(a,HCode)]
convert path tree = case tree of
   (Leaf n c) ->  [(c,path)]
   (Node n subtree1 subtree2) -> (convert (path++[L]) subtree1)
                              ++ (convert (path++[R]) subtree2)
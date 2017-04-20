data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

data OtherTree a = OtherNode a (Maybe (OtherTree a)) (Maybe (OtherTree a)) deriving (Show)

differentTree = OtherNode "parent" (Just (OtherNode "left child" Nothing Nothing))
                                   (Just (OtherNode "right child" Nothing Nothing))

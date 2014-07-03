data List a = Cons a (List a)
            | Nil
             deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

-- Java-esque nullable definition
data Tree a = Tree (Maybe a) (Maybe (Tree a)) (Maybe (Tree a)) deriving Show

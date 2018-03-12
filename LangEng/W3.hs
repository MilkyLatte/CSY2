-- 1. Given the data structure and Functor instance below, prove that it obeys the Functor laws.
data MaybeError a = Error String
                  | Just' a
-- fmap :: Functor f => (a -> b) -> (f a) -> (f b)
instance Functor MaybeError where
  fmap g (Error s) = (Error s)
  fmap g (Just' a) = (Just' (g a))
-- Q1
-- TWO LAWS
-- IDENTITY LAW:
--   fmap id (Error s)
-- = { def |fmap| }
--   Error s
--
-- It holds for Error s
--
--   fmap id (Just' s)
-- = { def 'fmap' }
--  Just' (id s)
-- = { def 'id'  }
--  Just' s
-- It holds for Just' s

-- COMPOSITION LAW:
--
-- fmap (f.g) (Error s)
-- = { def 'fmap'}
--   Error s
-- It holds for Error s

-- fmap (f.g) (Just' s)
-- = { def 'fmap'}
--  Just' ((f.g) s)
-- = { def '.'}
--  Just' (f (g s))

-- fmap f (fmap g (Just' s))
-- = { def 'fmap' }
-- fmap f (Just' (g s))
-- = { def 'fmap' }
--  Just' (f (g s))
--  Both values are equal, then the composition law also holds

-- Q2
data Tree a = Leaf
            | Branch a (Tree a) (Tree a)
instance Functor Tree where
  fmap g (Leaf) = Leaf
  fmap g (Branch a l r) = Branch (g a) (fmap g l) (fmap g r)

-- Q3 Functor laws
-- Identity Law:
-- fmap id (Leaf)
-- = { def 'fmap' }
--  Leaf
--
-- fmap id (Branch a l r)
-- = { def 'fmap'}
--  Branch (id a) (fmap id l) (fmap id r)
-- = { def 'id' }
--  Branch (a) (fmap id l) (fmap id r)
-- = {Induction Hipothesis}
--  Branch a l r
--
-- Compostion Law:
-- fmap (f.g) (Leaf)
-- = { def 'fmap' }
-- Leaf
--
-- fmap (f.g) (Branch a l r)
-- = { def 'fmap' }
-- Branch ((f.g) a) ((f.g) l) ((f.g) r)
-- = { def '.'}
-- Branch (f (g a)) (f (g l)) (f (g r))
--
-- fmap f (fmap g (Branch a l r))
-- = { def 'fmap' }
-- fmap f (Branch (g a) (g l) (g r))
-- = { def 'fmap' }
-- Branch (f (g a)) (f (g l) (f (g r))
-- Both values are equal, then the composition law also holds
-- Hence, the Tree is a Functor

-- Q4
-- The kind of Tree = (* -> *)

-- Q5

data Exception e a = Except e
                   | Result a

-- The kind of Exception is * -> * -> *
--Q6
-- The kind of any F that has a well defined Functor is (* -> *)




-- Exception is not a Functor because of its kind, Functors work strictly on
-- kinds (* -> *). In order to provide a Functor implementation of Exception
-- one must do:

-- instance Functor Exception (e a) where
--   fmap g (e) = e

-- Question 2 FIX POINTS
--1
data TreeF a k = EmptyF| BranchF a (k) (k)

data Fix f = In (f (Fix f))



--2
data ListF a k = EmptyyF
                | ConsF a k

instance Functor (TreeF a) where
    fmap f EmptyF = EmptyF
    fmap f (BranchF x l r) = BranchF x (f l) (f r)

-- 3. Define a recursive function that sums the values in a Tree of type Int. Also provide a TreeF -algebra
-- that performs the same task on Fix TreeF structures.

sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Branch a l r) = a + (sumTree l) + (sumTree r)



sumTreeF :: Fix (TreeF Int) -> Int
sumTreeF = cata alg where
  alg :: TreeF Int Int -> Int
  alg EmptyF = 0
  alg (BranchF x l r) = x + l + r

countLeaves :: Tree a -> Int
countLeaves Leaf = 1
countLeaves (Branch a l r) = (countLeaves l) + (countLeaves r)

countLeavesF :: Fix (TreeF Int) -> Int
countLeavesF = cata alg where
  alg :: TreeF Int Int -> Int
  alg EmptyF = 1
  alg (BranchF x l r) = l + r

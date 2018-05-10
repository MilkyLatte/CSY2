-- 1. Given the data structure and Functor instance below, prove that it obeys the Functor laws.


> data MaybeError a = Error' String
>                   | Just' a


--fmap :: Functor f ⇒ (a → b) → (f a) → (f b)
--fmap :: Functor f => (a -> b) -> (f a) -> (f b)

> instance Functor MaybeError where
>   fmap g (Error' s) = (Error' s)
>   fmap g (Just' a) = (Just' (g a))

-- The first functor law is the identity law which state that if we map the id
-- function over a functor, the functor that we get back should be the original as
-- the original functor.
-- fmap id (Error' s)
-- def { fmap }
-- Error' s
--
-- fmap id (Just' a)
-- def { fmap }
-- Just' (id a)
-- def { id }
-- Just' a
--
-- The second functor law is the Compostion law fmap (f . g) = fmap f . fmap g
-- fmap (f . g) Error' s
-- def { fmap }
-- Error' s
--
-- fmap (f . g) Just' a
-- def { fmap }
-- Just (f . g) a
-- def { . }
-- Just f (g a)
--
-- (fmap f . fmap g) Just' a
-- def { . }
-- fmap f ( fmap g (Just' a))
-- def { fmap }
-- fmap f (Just' (g a))
-- def { fmap }
-- Just' f (g a)
-- Both values imply the same thing so it works for Just' a

> data Tree a = Leaf
>             | Branch a (Tree a) (Tree a)
> instance Functor Tree where
>   fmap f (Leaf) = Leaf
>   fmap f (Branch a l r) = Branch (f a) (fmap f l) (fmap f r)

Proving the laws for tree:
ID Law:
fmap id Tree a -> Tree a

fmap id Leaf
= def { fmap }
  Leaf

fmap id (Branch a l r)
= def { fmap }
  Branch (id a) (fmap id l) (fmap id r)
= def { id }
  Branch a (fmap id l) (fmap id r)
= { Induction Hypothesis}
  Branch a l r

Compostion Law:
fmap (f . g) F = (fmap f . fmap g) F

fmap (f . g) Leaf
= def { fmap }
  Leaf

fmap (f . g) (Branch a l r)
= def ( fmap )
  Branch ((f . g) a) (fmap (f . g) l) (fmap (f . g) r)
= def ( . )
  Branch (f (g a)) (fmap (f . g) l) (fmap (f . g) r)

(fmap f . fmap g) (Branch a l r)
= def { . }
  fmap f (fmap g (Branch a l r))
= def { fmap }
  fmap f (Branch g a (fmap g l) (fmap g r))


4. * -> *
5.

> data Exception e a = Except e
>                    | Result a

kind = * -> * -> *

6. The kind for functor has to be * -> *
7. It is not a Functor since it does not have the kind that a functor requires

> instance Functor (Exception x) where
>   fmap f (Except a) = Except a
>   fmap f (Result x) = Result (f x)


Q2 FIXPOINTS
1. Provide a non-recursive datatype, TreeF, that corresponds to the recursive Tree data type, such that
Fix TreeF is equivalent to Tree.

> data TreeF a k = LeaF
>                | BranchF a (k) (k)

> data Fix f = In(f(Fix f))

> instance Functor (TreeF a) where
>  fmap f LeaF = LeaF
>  fmap f (BranchF a l r) = BranchF a (f l) (f r)

3. Define a recursive function that sums the values in a Tree of type Int. Also provide a TreeF-algebra
that performs the same task on Fix TreeF structures.

> cata :: Functor f => (f b -> b) -> Fix f -> b
> cata alg (In x)= (alg . fmap (cata alg)) x


> sumTree :: Tree Int -> Int
> sumTree Leaf = 0
> sumTree (Branch a l r) = a + (sumTree l) + (sumTree r)

> sumTreeF :: Fix(TreeF Int) -> Int
> sumTreeF = cata alg where
>  alg :: TreeF Int Int -> Int
>  alg LeaF = 0
>  alg (BranchF a l r) = a + l + r

> countLeaves :: Tree a -> Int
> countLeaves Leaf = 1
> countLeaves (Branch a l r) = countLeaves l + countLeaves r

> countLeavesF :: Fix(TreeF Int) -> Int
> countLeavesF = cata alg where
>  alg :: TreeF Int Int -> Int
>  alg LeaF = 1
>  alg (BranchF a l r) = l + r

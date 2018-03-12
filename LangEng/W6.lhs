> import Control.Monad
> import Control.Applicative hiding ((<$), (<*), (*>))
> import Prelude hiding ((<$), (<*), (*>))

> data Maybe' a = Nothing' | Just' a

> instance Functor Maybe' where
>  fmap f Nothing'  = Nothing'
>  fmap f (Just' a) = Just' (f a)

> instance Applicative Maybe' where
>   pure a = Just' a
>   Just' f  <*> Just' a = Just' (f a)
>   _        <*> _              = Nothing'

Q2
1.1.2.a
pure id <*> v = v
     pure (id) <*> Just v
   = { def |pure| }
     Just (id) <*> Just v
   = { def |(<*>)| }
     Just (id v)
   = { def |id| }
    Just v
1.1.2.b
pure f <*> pure x = pure (f x)
   = { def |pure| }
      Just f <*> Just x
   = { def |(<*>)| }
      Just (f x)

1.1.2.c
u <*> pure y = pure ($y) <*> u
   = { def |pure| }
     u <*> Just y

  case u = Just f
   = { def |(<*>)| }
    Just (f y)
  case u = Nothing
    = { def |(<*>)| }
    Nothing

  pure ($y) <*> u
   case u = Just f
    pure ($y) <*> Just f
   = { def |pure| }
    ($y) <*> Just f
   = { def |(<*>)| }
    Just (($y) f)
   = { def |$| }
    Just (f $ y)
  = { def |$| }
    Just (f y)
   case u = Nothing
  = { def |(<*>)| }
    Nothing


1.1.2.d
pure (.) <*> x <*> y <*> z = x <*> (y <∗> z)
   = { def |pure| }
    Just (.) <*> x <*> y <*> z
   = { def |(<*>)| }
    Just ((.) x) <*> y <*> z
   = { def |(<*>)| }
    Just (((.)x) y) <*> z
   = { def |(<*>)| }
    Just ((((.)x) y )z)
  = { def | (.) | }
    Just ((x . y) z)
  = { def |(.)| }
    Just (x (y z))
 x <*> (y <∗> z)
  = { def |(<*>)| }
    x <*> Just (y z)
  = { def |(<*>)| }
    Just (x (y z))


1.2

> data List a = Empty | Cons a (List a)
>               deriving Show

1.2.1

> instance Functor List where
>  fmap f (Empty)     = Empty
>  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

1.2.2

> (<$) :: a -> List b -> List a
> x <$ Empty       = Empty
> x <$ (Cons y ys) = Cons x ((fmap.const) x ys)

1.2.3

> conc :: List a -> List a -> List a
> conc x Empty          = Empty
> conc Empty x          = x
> conc (Cons x Empty) y = Cons x y
> conc (Cons x xs) y    = Cons x (conc xs (y))



> instance Applicative List where
>   pure x = Cons x (Empty)
>   _ <*> Empty = Empty
>   Empty <*> x = Empty
>   Cons x xs <*> Cons y ys = conc (Cons (x y) (x <$> ys)) (xs <*> Cons y ys)


1.2.4

> (<*) :: Applicative f => f a -> f b -> f a
> x <* y = const <$> x <*> y

1.2.5

> (*>) :: Applicative f => f a -> f b -> f b
> x *> y = flip const <$> x <*> y

1.2.6

> instance Alternative List where
>   empty = Empty
>   (<|>) = conc

> instance Monoid (List a) where
>  mempty = Empty
>  Empty `mappend`     y  = y
>  Cons x xs `mappend` ys = Cons x (xs `mappend` ys)

1.2.7
a) Cons :: a -> List a -> List a
b) Cons(<$>):: Functor f =>
     List ((a -> b) -> f a -> f b) -> List ((a -> b) -> f a -> f b)
c) (<$) :: Functor f => a -> f b -> f a
d) Cons(<$) :: List (a -> List b -> List a) -> List (a -> List b -> List a)
e) Cons <$> Just 3 <*> Just (Cons 2 Empty) :: Num a => Maybe (List a)
f) Cons <$> Just 3 :: Num a => Maybe (List a -> List a)
g) Cons <$> (Cons 5 (Empty)) :: Num a => List (List a -> List a)
h) (Cons 3 <$>) :: (Num a, Functor f) => f (List a) -> f (List a)

1.2.8
a) Just (Cons 3 (Cons 2 Empty))
b) Just (Cons (Cons 1 Empty))
c) Cons (Just 5) (Cons (Just 2) Empty)
d) Just <$> Cons 5 (Cons 2 Empty)
e) Empty
f) Empty

1.2.9

> liftATwo :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> liftATwo f a y = f <$> a <*> y

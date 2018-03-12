data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap f Nothing'  = Nothing'
  fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
  pure a = Just' a
  Just' f  <*> Just' a = Just' (f a)
  Nothing'        <*> _              = Nothing'
  _               <*> Nothing'       = Nothing'

-- Q2
-- pure id <*> v = v
--     pure (id) <*> Just v
--   = { def |pure| }
--     Just (id) <*> Just v
--   = { def |(<*>)| }
--     Just (id v)
--   = { def |id| }
--    Just v

-- pure f <*> pure x = pure (f x)
--   = { def |pure| }
--      Just f <*> Just x
--   = { def |(<*>)| }
--      Just (f x)
--
--
-- u <*> pure y = pure ($y) <*> u
--   = { def |pure| }
--     u <*> Just y
--
--  case u = Just f
--   = { def |(<*>)| }
--    Just (f y)
--  case u = Nothing
--    = { def |(<*>)| }
--    Nothing
--
--  pure ($y) <*> u
--   case u = Just f
--    pure ($y) <*> Just f
--   = { def |pure| }
--    ($y) <*> Just f
--   = { def |(<*>)| }
--    Just (($y) f)
--   = { def |$| }
--    Just (f $ y)
--  = { def |$| }
--    Just (f y)
--

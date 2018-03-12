data State s a = State (s -> (s, a))

runState :: State s a -> s -> (s, a)
runState (State prog) = prog

execState :: State s a -> s -> s
execState prog = \s -> fst (runState prog s)

{-
addTwo :: State Int Int
addTwo = do s <- get


main :: IO a
main = do putStrLn "Hello World"
          s <- getLine
          return s

main = putStrLn "Hello World" >>= (\_ -> getLine >>= (\s -> return s))

getInt :: Db -> Maybe Int



(>>=) :: m a -> (a -> m b) -> m b

-}



evalState :: State s a -> s -> a
evalState prog = \s -> snd (runState prog s)

instance Functor (State s) where
  fmap f s = s >>= (return . f)

instance Applicative (State s) where
  pure x = State (\s -> (s, x))
  (State prog) <*> (State p) = State (\s -> (s, let (s', x ) = p s in (snd (prog s')) x ))

instance Monad (State s) where
  return x = State (\s -> (s, x))
  State p >>= f = State (\s -> let (s', x ) = p s in runState (f x) s')



get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\_ -> (x ,()))

--1
add :: Int -> Int -> State Int Int
add x y = State (\s -> ((s + x), (x + y)))

--2
operationMlt :: Int -> Int -> State Int Int
operationMlt x y = State (\s -> (s + y, x * y))

--3

runOp :: Int -> Int -> (Int, Int)
runOp x y = runState (operationMlt x y) 0

--1.1.1
type MiniImp = State (Int, Int)

getX :: MiniImp Int
getX = fst <$> get

getY :: MiniImp Int
getY = snd  <$> get

setX :: Int -> MiniImp ()
setX x = do
        (_, y) <- get
        put(x, y)

setY :: Int -> MiniImp ()
setY y = do
        (x, _) <- get
        put (x, y)

while :: ((Int,Int) -> Bool) -> ((Int,Int) -> (Int,Int)) -> MiniImp ()
while p f =
         do
            xy <- get
            if p xy
            then do
             put (f xy)
             while p f
            else return ()

-- while p f = get >>= (\xy -> if p xy then put (f xy) >> while p f else return ())
-- while p f = State (\s -> (s, s)) >>= (\xy -> if p xy then put (f xy) >> while p f else return ())
-- while p f = State (\s -> (s, s)) >>= (\xy -> if p xy then (State (\_ -> ((f xy),()))) >> while p f else return ())
-- while p f = State (\s -> (s, s)) >>= (\xy -> if p xy then (State (\_ -> ((f xy),()))) >> while p f else State (\s -> (s, ()))
-- while p f = State (\s -> let (s', x ) = runState mx s in runState (g x) s')
--   where mx = State (\s -> (s, s))
--         g = (\xy -> if p xy then (State (\_ -> ((f xy),()))) >> while p f else State (\s -> (s, ()))
--
-- while p f = State (\s -> runState (g s) s)
--   where g = (\xy -> if p xy then (State (\_ -> ((f xy),()))) >> while p f else State (\s -> (s, ()))
--
-- while p f = State (\s -> (if p s then runState mx s else (s, ()))
--   where mx = (State (\_ -> ((f s),()))) >> while p f)

-- while p f = State (\xy -> (if p xy then runState (while p f) (f xy) else (xy, ())))



firstCnd :: (Int, Int) -> Bool
firstCnd (x , y) = if y /= 0
  then True
  else False
secondCnd :: (Int, Int )-> (Int, Int)
secondCnd (x, y) = ((x*y), (y - 1))

fact' :: MiniImp Int
fact' = do
  while firstCnd secondCnd
  getX

getValue :: ((Int, Int), Int)-> Int
getValue (_, y) = y

fact :: Int -> Int
fact x = getValue (runState fact' (1, x))

triangCnd:: (Int, Int) -> (Int, Int)
triangCnd (x, y) = ((x + y), (y - 1))

triang' :: MiniImp Int
triang' = do
  while firstCnd triangCnd
  getX

triang :: Int -> Int
triang x = getValue (runState triang'(0,x))

--1.2
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a b = (f <$> a) <*> b
--(>>=) :: Monad m => m a -> (a -> m b) -> m b
liftM2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = a >>= (\x -> f x <$> b)

liftM2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2'' f a b = a >>= (\x -> b >>= (\y -> return (f x y)))

liftM2''' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2''' f a b = do
  x <- a
  y <- b
  return (f x y)

--Q1
data Fix f = In (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In x) = (alg . fmap (cata alg)) x

--Q2
data Robot' k = F Int k | L k | R k | Stop

instance Functor (Robot') where
  fmap f Stop = Stop
  fmap f (F a b) = F a (f b)
  fmap f (L a) = L (f a)
  fmap f (R a) = R (f a)


-- Q3 Functor Laws
-- fmap id (Stop)
-- { def 'Stop' }
-- Stop
-- Holds for Stop

-- fmap id (F a b)
-- { def 'fmap' }
-- F a (f b)
--



testRobot = In (F 5 (In (L (In (F 10 (In (Stop)))))))
-- Q4 Distance
distance :: Fix Robot' -> Int
distance = cata alg where
  alg :: Robot' Int -> Int
  alg Stop = 0
  alg (F a x) =  a + x
  alg (L a) = 0 + a
  alg (R a) = 0 + a

-- Q5 Distance on direction

data Direction =  N
                  | S
                  | E
                  | W
                  deriving (Eq, Show)

rightTurn :: Direction -> Direction
rightTurn N = E
rightTurn E = S
rightTurn S = W
rightTurn W = N

leftTurn :: Direction -> Direction
leftTurn N = W
leftTurn W = S
leftTurn S = E
leftTurn E = N


data Robot'' k = F' Integer k | L' k | R' k | Stop' k| Start

instance Functor Robot'' where
  fmap f (Start)     = Start
  fmap f (Stop' a)   = Stop' (f a)
  fmap f (F' a c)    = (F' a (f c))
  fmap f (L'  a)     = L' (f a)
  fmap f (R'  a)     = R' (f a)


direction :: Fix Robot'' -> (Integer, Direction)
direction  = cata alg where
  alg :: Robot'' (Integer, Direction) -> (Integer, Direction)
  alg (Start)        = (0, N)
  alg (Stop' (x, y)) = (x, y)
  alg (F' a  (x, y))
           | y == N = (x + a, y)
           | otherwise = (x, y)
  alg (L'  (x,y)) = (x, leftTurn y)
  alg (R'  (x,y)) = (x, rightTurn y)


finalDirection :: Fix Robot'' -> Integer
finalDirection a =  ((\(x, y) -> x) . direction) a

eg = In (Stop' (In (F' 5 (In (L' (In (F' 10 (In (L' (In (L' (In (L' (In (F' 1000 (In( Start)))))))))))))))))


-- Q6
fromOrigin :: Fix Robot'' -> ((Integer, Direction), (Integer, Integer))
fromOrigin = cata alg where
   alg :: Robot'' ((Integer, Direction), (Integer, Integer)) -> ((Integer, Direction), (Integer, Integer))
   alg (Start) = ((0, N) , (0,0))
   alg (F' a ((c,d), (x, y)))
                  | d == N = ((c,d), (x, y + a))
                  | d == S = ((c,d), (x, y - a))
                  | d == E = ((c,d), (x + a, y))
                  | d == W = ((c,d), (x - a, y))
   alg (L' ((c,d) , (x, y)))  = ((c, leftTurn d) , (x,y))
   alg (R' ((c,d) , (x, y)))  = ((c, rightTurn d) , (x,y))
   alg (Stop' ((c,d), (x, y))) = ((c,d) , (x, y))

fromOriginFinal :: Fix Robot'' -> Float
fromOriginFinal a = ((\((c,d), (x,y)) -> sqrt(fromIntegral ((x*x) + (y*y)))) . fromOrigin) a

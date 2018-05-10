--1
data Robot = F Int Robot
            |L Robot
            |R Robot
            |Stop
            deriving Show

--1.1.2.2
-- Write a function shich calculates the total distance travelled
-- by the Robot
disTrav :: Robot -> Int
disTrav (F x y) = x + (disTrav y)
disTrav (L x)   = disTrav x
disTrav (R x)   = disTrav x
disTrav Stop    = 0
--1.1.2.3
-- Write a fucntion which calculates the distance the Robot was first facing
data Direction = N
               | E
               | W
               | S
               deriving Eq

rightTurn:: Direction -> Direction
rightTurn N = E
rightTurn E = S
rightTurn S = W
rightTurn W = N

leftTurn:: Direction -> Direction
leftTurn N = W
leftTurn W = S
leftTurn S = E
leftTurn E = N

dir :: Robot -> Direction -> Direction
dir (F x y) d = d
dir (L r) d   = leftTurn d
dir (R r) d   = rightTurn d
dir Stop  d   = d

disTravDir :: Direction -> Robot -> Direction -> Int
disTravDir s (F x y) c
                    | s == c = x + (disTravDir s y c)
                    | otherwise = disTravDir s y c
disTravDir s (L d) c = disTravDir s d (dir (L d) c)
disTravDir s (R d) c = disTravDir s d (dir (R d) c)
disTravDir s Stop  c = 0

data Location = Coord (Int, Int)


finalPosition :: Robot -> Direction -> Location -> Float
finalPosition (F d r) dire (Coord(x,y))
                                   | dire == N = finalPosition r dire (Coord(x, y+d))
                                   | dire == S = finalPosition r dire (Coord(x, y-d))
                                   | dire == E = finalPosition r dire (Coord(x+d, y))
                                   | dire == W = finalPosition r dire (Coord(x-d, y))
finalPosition (L r) dire xy = finalPosition r (dir (L r) dire) xy
finalPosition (R r) dire xy = finalPosition r (dir (R r) dire) xy
finalPosition Stop dire (Coord(x,y))  = sqrt(fromIntegral(x*x + y*y))



-- 1.2 Cooking master
-- We will attempt to model a basic set of recipes. However, to make life easier we shall initially only include
-- potatoes. This will be done using a shallow embedding, where the semantic output will be the properties of
-- the potatoes:
-- 1. Time taken: The time it takes, as an Int, to prepare the potato .
-- 2. Weight: The weight of potatoes prepared as an Int.
-- 3. Cooked: Whether the potatoes have been cooked, as a Bool.
-- 4. Description: Information about the potatoes as a String.

-- 1. Create a value encoding a single potato, with the below type, which represents the various semantic
-- outputs of a potato dish:
-- potato :: (Int,Int, Bool, String)
-- The data type (Int,Int, Bool, String) in this case relates directly to (time taken, weight, cooked, description),
-- and potato will encode a single potato into this semantics. This means that the we would assume the
-- time taken to be nothing, we will only have one potato with weight 3, it won’t be cooked and the only
-- description we can give will be “potato”.
type Potato = (Integer, Integer, Bool, String)

onePotato = (0, 3, False, "")
-- (a) peel (takes 2 mins for each potato and adds “peeled” to the description)
peel :: Potato -> Potato
peel (t, w, c, d) = (t + 2, w, c, d ++ " peeled")
-- (b) roast (takes 70 mins, makes them cooked and adds “roasted” to description)
roast :: Potato -> Potato
roast (t, w, c, d) = (t + 70, w, True, d ++" roasted")
-- (c) boil em (takes 25 mins, makes then cooked and adds “boiled” to the description)
boilEm :: Potato -> Potato
boilEm (t, w, c, d) = (t + 25, w, True, d ++" boiled")
-- (d) mash em (takes 1 min per potato and adds “mashed” to the description)
mashEm :: Potato -> Potato
mashEm (t, w, c, d) = (t + 1, w, c, d ++" mashed")
-- (e) stick em in a stew (takes 120 mins, makes them cooked and adds “stewed” to the description)
stickEm :: Potato -> Potato
stickEm (t, w, c, d) = (t + 120, w, True, d ++" stewed")

-- 3. Create a function that lets you mix two sets of potatoes, this should combine the time taken and
-- weights, become uncooked if either is uncooked and combine the two descriptions. The type signature
-- is given below:
mix :: (Int,Int, Bool, String) -> (Int,Int, Bool, String) -> (Int,Int, Bool, String)
mix (t1, w1, c1, d1) (t, w, c, d) = (t1 + t, w1 + w, c1 && c, d1++d)

data ListF a k = EmptyF | ConsF a k deriving Show


data Fix f = In(f(Fix f))

instance Functor (ListF n) where
  fmap f EmptyF = EmptyF
  fmap f (ConsF n x) = ConsF n (f x)

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg (In x) = (alg . fmap(cata alg)) x

sumL :: Fix(ListF Int) -> Int
sumL = cata alg where
  alg :: ListF Int Int -> Int
  alg EmptyF = 0
  alg (ConsF n x) = x + n

toListF :: [a] -> Fix(ListF a)
toListF = foldr f k where
  f :: a -> Fix(ListF a) -> Fix(ListF a)
  f x xs = In (ConsF x xs)
  k :: Fix(ListF a)
  k = In(EmptyF)

fromListF :: Fix(ListF a) -> [a]
fromListF = cata alg where
  alg :: ListF a [a] -> [a]
  alg EmptyF = []
  alg (ConsF n xs) = n : xs

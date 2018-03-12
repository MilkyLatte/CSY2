-- Question 1




data Robot = F Int Robot
            |L Robot
            |R Robot
            |Stop
-- Question 2
distrav :: Robot -> Int
distrav (F d b)  = distrav (b) + d
distrav (L b)    = distrav b
distrav (R b)    = distrav b
distrav (Stop)      = 0
-- Question 3
data Direction =  N
                | S
                | E
                | W
                deriving Eq
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


direction :: Robot -> Direction -> Direction
direction (F d b) w = w

direction (L b) w = leftTurn w

direction (R b) w = rightTurn w

direction (Stop) w = w

distravDir :: Robot -> Direction -> Direction -> Int -> Int
distravDir (F d b) N N dis = distravDir b (direction b N) N (dis+d)
distravDir (F d b) E N dis = distravDir b (direction b E) N dis
distravDir (F d b) S N dis = distravDir b (direction b S) N dis
distravDir (F d b) W N dis = distravDir b (direction b W) N dis

distravDir (F d b) E E dis = distravDir b (direction b E) E (dis+d)
distravDir (F d b) N E dis = distravDir b (direction b N) E dis

distravDir (F d b) S E dis = distravDir b (direction b S) E dis
distravDir (F d b) W E dis = distravDir b (direction b W) E dis

distravDir (F d b) E W dis = distravDir b (direction b E) W dis
distravDir (F d b) N W dis = distravDir b (direction b N) W dis
distravDir (F d b) S W dis = distravDir b (direction b S) W dis
distravDir (F d b) W W dis = distravDir b (direction b W) W (dis+d)

distravDir (F d b) E S dis = distravDir b (direction b E) S dis
distravDir (F d b) N S dis = distravDir b (direction b N) S dis
distravDir (F d b) S S dis = distravDir b (direction b S) S (dis+d)
distravDir (F d b) W S dis = distravDir b (direction b W) S dis

distravDir (R b) dir orig dis = distravDir b (direction b dir) orig dis
distravDir (L b) dir orig dis = distravDir b (direction b dir) orig dis

distravDir (Stop) dir orig dis = dis

--distravDir ((F 10 ((L(R(R(F 10 (R(F 30(Stop)))))))))) N N 0
--finDist ((F 10 ((L(R(R(F 10 (R(F 30(Stop)))))))))) N (Coord(0,0))
-- Design a function that calculates the distance the robot ends up away from its starting position, in a
--straight line, with the output as a Float.

data Location = Coord(Int,Int)


finDist :: Robot -> Direction -> Location -> Float
finDist (F d b) N (Coord(x, y)) = finDist b (direction b N) (Coord(x, y+d))
finDist (F d b) W (Coord(x, y)) = finDist b (direction b W) (Coord(x-d, y))
finDist (F d b) S (Coord(x, y)) = finDist b (direction b S) (Coord(x, y-d))
finDist (F d b) E (Coord(x, y)) = finDist b (direction b E) (Coord(x+d, y))

finDist (L b) d (Coord(x, y)) = finDist b (direction b d) (Coord(x, y))

finDist (R b) d (Coord(x, y)) = finDist b (direction b d) (Coord(x, y))


finDist (Stop) d (Coord(x, y)) = sqrt (fromIntegral((x*x) + (y*y)))


type Potato = (Int, Int, Bool, String)
-- Q2
-- potato :: (Int, Int, Bool, String)
-- potato
peel:: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
peel (a, b, c, d) = (a+2, b, c, (d  ++ "peeled"))

roast:: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
roast (a,b,c,d) = (a+70, b, True, (d ++ "roasted"))

boileEm:: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
boileEm (a,b,c,d) = (a+25, b, True, (d ++ "boiled"))

mashEm:: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
mashEm (a,b,c,d) = (a+1, b, c, (d ++ "mashed"))

stickEmInaStew:: (Int, Int, Bool, String) -> (Int, Int, Bool, String)
stickEmInaStew (a,b,c,d) = (a + 120, b, True, (d ++ "stewed") )

mix :: (Int,Int, Bool, String) -> (Int,Int, Bool, String) -> (Int,Int, Bool, String)
mix (a,b,c,d) (e,f,g,h) = (a+e, b+f, c&&g, (d ++ " and " ++ h))

--Q3

-- GPL is Turing-complete, meaning they can compute any function
-- and is broadly applicable across application domains. On the other hand, DSL is
-- specialised for specific uses and may not be Turing-complete. GPL can be split
-- into imperative languages, where each computational step is clearly stated, and
-- declarative. Declarative can have either a functional paradigm (e.g. Haskell),
-- meaning it is a pure language that compose functions together for computation,
-- or a logic paradigm
--

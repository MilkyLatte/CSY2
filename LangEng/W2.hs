import Data.List

data Animal = Lion
          | Tiger
          | Gazelle
          | Ant
      deriving (Eq, Show)


data Safari = Quadrant Safari Safari Safari Safari
           | Territory [Animal]
           | Tres Safari Safari Safari


--Q1
-- Shallow
-- (NumberofAnimals, Animals in that territory)
type Safarii = [Int]

territory :: [Animal] -> Safarii
territory a = [length a]

quadrant:: Safarii -> Safarii -> Safarii -> Safarii -> Safarii
quadrant a b c d = a ++ b ++ c ++ d

numTerritories :: Safarii -> Int
numTerritories s = length s

maxNum :: Safarii -> Int
maxNum s = maximum s

--Q2
maxAnimals :: Safari -> Int
maxAnimals (Quadrant a b c d) = maximum [maxAnimals a, maxAnimals b, maxAnimals c, maxAnimals d]
maxAnimals (Territory a) = length a

territories :: Safari -> Int -> Int
territories (Quadrant a b c d) x = (sum [territories a 0, territories b 0, territories c 0, territories d 0])
territories (Territory a) x = x + 1

tt :: Safari -> Int
tt (Territory a) = 1
tt (Quadrant a b c d) = tt a + tt b + tt c + tt d

testSafari = Quadrant(Territory [Tiger, Lion, Ant]) (Quadrant (Territory [Tiger, Lion])  (Territory [Tiger, Lion])  (Territory [Tiger, Lion]) (Territory [Tiger, Lion]))  (Territory [Tiger, Lion])  (Territory [Tiger, Lion])

--Q3
trio :: Safari -> Int
trio (Territory a) = 1
trio (Tres a b c) = trio a + trio b + trio c

tres :: Safarii -> Safarii -> Safarii -> Safarii
tres a b c  = a ++ b ++ c

--Q4
animals :: Safari -> [Animal]
animals (Territory a) = nub a
animals (Quadrant a b c d) = nub (animals a ++ animals b ++ animals c ++ animals d)


-- Shallow
type Safari3 = [(Int, [Animal])]
territory3 :: [Animal] -> Safari3
territory3 a = [(length a, a)]

quadrant3 :: Safari3 -> Safari3 -> Safari3 -> Safari3 -> Safari3
quadrant3 a b c d = a ++ b ++ c ++ d

maxAnimals3 :: Safari3 -> Int
maxAnimals3 ((x1,x2) : xs) = maximum ((x1 : [] ++ (maxAnimals3 xs : [])))
maxAnimals3 [] = 0

animalsInSafari :: Safari3 -> [Animal]
animalsInSafari ((x1,x2) : xs) = x2 ++ animalsInSafari xs
animalsInSafari [] = []

testSafari3 = quadrant3(territory3 [Lion, Tiger, Ant, Ant]) (territory3 [Lion, Tiger, Ant, Ant, Gazelle]) (territory3 [Lion, Lion,Lion,Lion]) (territory3 [Lion, Tiger, Gazelle, Ant])

--Q6
data Safari1 = Territory1 [Animal]
           | Plot [Safari1]
           | Fraction Double Safari1
           deriving (Eq, Show)
--Question 7
numTerritories1 :: Safari1 -> Int
numTerritories1 (Plot []) = 0
numTerritories1 (Plot (x:xs)) = (numTerritories1 x) + (numTerritories1 (Plot xs))
numTerritories1 (Territory1 a) = 1
numTerritories1 (Fraction d c) = numTerritories1 c

maxNumAnimals :: Safari1 -> [Int]
maxNumAnimals (Plot []) = []
maxNumAnimals (Plot (x:xs))  = (maxNumAnimals x ++ maxNumAnimals (Plot xs))
maxNumAnimals (Fraction d c) = maxNumAnimals c
maxNumAnimals (Territory1 a) = [length a]

maxNumAnimalsFinal :: Safari1 -> Int
maxNumAnimalsFinal a = maximum (maxNumAnimals a)

listAnimals :: Safari1 -> [Animal]
listAnimals (Plot []) = []
listAnimals (Plot (x:xs)) = nub(listAnimals x ++ listAnimals (Plot xs))
listAnimals (Fraction d c) = listAnimals c
listAnimals (Territory1 a) = a

--Q8
quad :: Safari1 -> Safari1 -> Safari1 -> Safari1 -> Safari1
quad (Fraction a b) (Fraction c d) (Fraction e f) (Fraction g h) = Plot [Fraction 0.25 b, Fraction 0.25 d, Fraction 0.25 f, Fraction 0.25 h]
quad (Territory1 a) (Territory1 b) (Territory1 c) (Territory1 d) = Plot [Fraction 0.25 (Territory1 a), Fraction 0.25 (Territory1 b), Fraction 0.25 (Territory1 c), Fraction 0.25 (Territory1 d)]
-- Q9
triple :: Safari1 -> Safari1 -> Safari1 -> Safari1
triple (Fraction a b) (Fraction c d) (Fraction e f)  = Plot [Fraction (1/3) b, Fraction (1/3) d, Fraction (1/3) f]
triple (Territory1 a) (Territory1 b) (Territory1 c)  = Plot [Fraction (1/3) (Territory1 a), Fraction (1/3) (Territory1 b), Fraction (1/3) (Territory1 c)]

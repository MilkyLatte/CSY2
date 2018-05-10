import Data.List

data Animal = Ant
             |Gazelle
             |Lion
             |Tiger
             deriving (Eq, Show)

--Shallow Embedding
type SafariShallow = [Int]
territory :: [Animal] -> SafariShallow
territory x = [length x]

quadrant :: SafariShallow -> SafariShallow -> SafariShallow -> SafariShallow -> SafariShallow
quadrant a b c d = a++b++c++d

triplet :: SafariShallow -> SafariShallow -> SafariShallow -> SafariShallow
triplet a b c = a++b++c

numTerritories :: SafariShallow -> Int
numTerritories s = length s

maxNum :: SafariShallow -> Int
maxNum s = maximum s

--Deep Embedding
data Safari  = Quadrant Safari Safari Safari Safari
            | Territory [Animal]
            | Trie Safari Safari Safari
            deriving (Show)

maxAnimals :: Safari -> Int
maxAnimals (Quadrant a b c d) = maximum[maxAnimals a, maxAnimals b, maxAnimals c, maxAnimals d]
maxAnimals (Trie a b c)       = maximum[maxAnimals a, maxAnimals b, maxAnimals c]
maxAnimals (Territory a)      = length a

numTerritories1 :: Safari -> Int
numTerritories1 (Quadrant a b c d) = sum [numTerritories1 a, numTerritories1 b, numTerritories1 c, numTerritories1 d]
numTerritories1 (Trie a b c)       = sum [numTerritories1 a, numTerritories1 b, numTerritories1 c]
numTerritories1 (Territory a)      = 1

whatAnimals :: Safari -> [Animal]
whatAnimals (Quadrant a b c d) = nub (whatAnimals a ++ whatAnimals b ++ whatAnimals c ++ whatAnimals d)
whatAnimals (Territory a)      = nub(a)






data Mountain = Arboles Int
              | Tierra Int
              | Fauna [Animales]

data Animales = Zorro
              | Ardilla
              | Aguila
              deriving Show

cuantosAnimales :: Mountain -> Int
cuantosAnimales (Fauna a) = length(a)

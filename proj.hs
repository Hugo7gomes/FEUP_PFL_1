import Data.List
import Data.Ord
import System.IO

type Factor = (Int, [(Char,Int)])
type Poli = [Factor]



sortLitAlg :: (Char,Int) -> (Char,Int) -> Ordering
sortLitAlg lit1 lit2 | snd lit1 < snd lit2 = LT
                     | snd lit1 > snd lit2 = GT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 < fst lit2)) = LT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 >= fst lit2)) = GT

addExpo :: [(Char,Int)] -> [(Char,Int)]
addExpo (x:xs) = [(fst x, sum ([snd y | y <- xs, (fst x) == (fst y)] ++ [snd x]))] ++ addExpo [z | z <- xs, (fst x) /= (fst z)]
addExpo [] = []



sortLiteral :: [(Char,Int)] -> [(Char,Int)]
sortLiteral l = reverse (sortBy sortLitAlg expoAdded)
                where expoAdded = addExpo l

sortFactor :: Factor -> Factor -> Ordering
sortFactor fac1 fac2 |  degree1 <= degree2 = LT
                     |  degree1 > degree2 = GT
                     where degree1 = snd ( head (snd fac1))
                           degree2 = snd ( head (snd fac2))

sortPoli :: [Factor] -> [Factor]
sortPoli l = reverse(sortBy sortFactor [(fst x, sortLiteral(snd x)) | x <- l, fst x /= 0])


normalize :: Poli -> Poli
normalize polinom = addFactor l1
                    where l1 = sortPoli polinom

addFactor :: [Factor] -> [Factor]
addFactor (x:xs) = [(sum([fst y | y <- xs, (snd x) == (snd y)] ++ [fst x]) ,snd x)] ++ addFactor [z | z <- xs, (snd x) /= (snd z)]
addFactor [] = []






addTwoPolis :: Poli -> Poli -> Poli
addTwoPolis pol1 pol2 = normalize (pol1 ++ pol2)





multiplicatePolis :: Poli -> Poli -> Poli
multiplicatePolis pol1 pol2 = normalize [((fst x) * (fst y), (snd x) ++ (snd y)) | x <- pol3, y <- pol4]
                  where pol3 = normalize pol1
                        pol4 = normalize pol2






multiplicateCoef :: Poli -> Char -> Poli
multiplicateCoef (x:xs) var = [(product([fst x] ++ [snd y | y <- (snd x), (fst y) == var]), snd x)] ++ multiplicateCoef xs var
multiplicateCoef [] var = []


reduceDegree :: Factor -> Char -> Factor
reduceDegree fac1 var = (fst fac1, [(fst x, snd x - 1) | x <- snd fac1, (fst x) == var] ++ [x | x <- snd fac1, (fst x) /= var])

eliminate0Degree :: Factor -> Factor
eliminate0Degree fac1 = (fst fac1, [(' ', 0) | x <- snd fac1, (snd x) == 0] ++ [x | x<- snd fac1, (snd x) /= 0])

sortByVar :: (Char,Int) -> Char -> Ordering
sortByVar lit1 var |(fst lit1 == var) = GT
                   |(fst lit1 /= var) = LT


eliminateTerms :: Factor -> Char -> Bool
eliminateTerms x var = foldr (||) False [var == (fst y)| y <- (snd x)]



derivatePoli :: Poli -> Char -> Poli
derivatePoli pol1 var = normalize [eliminate0Degree x | x <- pol5]
                        where pol5 = [x | x <- pol4, eliminateTerms x var]
                              pol4 = normalize [reduceDegree x var | x <- pol3]
                              pol3 = multiplicateCoef pol2 var
                              pol2 = normalize pol1

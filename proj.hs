import Data.List
import Data.Ord
import System.IO

type Factor = (Int, [(Char,Int)])
--type Coeficient = Int
--type LiteralDegree = (Char,Int)

type Poli = [Factor]

sortLitAlg :: (Char,Int) -> (Char,Int) -> Ordering
sortLitAlg lit1 lit2 | snd lit1 < snd lit2 = LT
                     | snd lit1 > snd lit2 = GT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 < fst lit2)) = LT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 >= fst lit2)) = GT



sortLiteral :: [(Char,Int)] -> [(Char,Int)]
sortLiteral l = reverse (sortBy sortLitAlg l)

sortFactor :: Factor -> Factor -> Ordering
sortFactor fac1 fac2 |  degree1 <= degree2 = LT
                     |  degree1 > degree2 = GT
                     where degree1 = snd ( head (snd fac1))
                           degree2 = snd ( head (snd fac2))

sortPoli :: [Factor] -> [Factor]
sortPoli l = reverse(sortBy sortFactor [(fst x, sortLiteral(snd x)) | x <- l, fst x /= 0])


normalize :: Poli -> Poli
normalize polinom = addFactor [head l1] (tail l1)
                    where l1 = sortPoli polinom

addFactor :: [Factor] -> [Factor] -> [Factor]
addFactor l1 l2 = if length l3  > 1
                  then [(sum[fst x | x <- l2, snd(head l1) == snd x] + fst(head l1) , snd(head l2))] ++ addFactor [head l3] (tail l3)
                  else if ((0 < length l3) && (length l3 < 2))
                  then [(sum[fst x | x <- l2, snd(head l1) == snd x] + fst(head l1) , snd(head l2))] ++ [head l3]
                  else [(sum[fst x | x <- l2, snd(head l1) == snd x] + fst(head l1) , snd(head l2))] ++ []
                  where l3 = [y | y <-l2, snd(head l2) /= snd y]


addTwoPolis :: Poli -> Poli -> Poli
addTwoPolis pol1 pol2 = normalize (pol1 ++ pol2)


multiplicatePolis :: Poli -> Poli -> Poli
multiplicatePolis pol1 pol2 = [((fst x) * (fst y), (snd x) ++ (snd y)) | x <- pol3, y <- pol4]
                  where pol3 = normalize pol1
                        pol4 = normalize pol2
{-
derivateFactor :: Factor -> Char -> Factor
derivateFactor fac1 var =
derivateFactor

derivatePoli :: Poli -> Char -> Poli
derivatePoli pol1 var = normalize [derivateFactor x var | x <- pol2]
                          where pol2 = normalize pol1
                          -}

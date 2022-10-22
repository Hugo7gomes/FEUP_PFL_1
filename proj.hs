module Proj where

import Data.List
import Data.Ord
import Data.Char
import System.IO
import Data.String



type Factor = (Int, [(Char,Int)])
type Poli = [Factor]


splitString :: String -> [String]
splitString poly | (take 1 poly) == "-" = [take 1 poly] ++ splitString (drop 1 poly)
                 | otherwise = case dropWhile (\x -> x == '+' || x == '-') poly of
                                    "" -> []
                                    s' -> w : splitString s''
                                        where (w, s'') = break (\x -> x =='+' || x =='-') s'


removeSpaces :: [String] -> [String]
removeSpaces x = [filter (\xs -> (xs /=' ')) y | y <- x]


createMonomyal :: [String] -> [String]
createMonomyal (x:xs:xss) |x == "-" = [x ++ xs] ++ createMonomyal xss
                          |x == "+" = [xs] ++ createMonomyal xss
                          |otherwise = [x] ++ createMonomyal ([xs] ++ xss)
createMonomyal (x:[]) = [x]
createMonomyal [] = []


createLiteral :: String -> [(Char, Int)]
createLiteral (x:xs:xss) | isLetter xs = [(x, 1)] ++ createLiteral ([xs] ++ xss)
                         | xs == '^' = [(x, read (takeWhile (isDigit) xss) :: Int)] ++ createLiteral (dropWhile (isDigit) xss)
createLiteral (x:[]) = [(x, 1)]
createLiteral [] = []



takeCoef :: String -> Factor
takeCoef (x:xs)
  | isDigit x = ((read ([x] ++ (takeWhile (isDigit) xs)) :: Int), createLiteral (dropWhile (isDigit) xs))
  | x == '-' = if isDigit (head xs) then ((read ([x] ++ (takeWhile (isDigit) xs)) :: Int), createLiteral (dropWhile (isDigit) xs))
              else (-1, createLiteral xs)
  | isLetter x = (1, createLiteral ([x] ++ xs))


createPoly :: String -> Poli
createPoly x = [(fst z, [(' ', 0)]) | z <- poly, (snd z) == []] ++ [z | z <- poly, (snd z) /= []]
            where poly = [takeCoef mono | mono <- monomyal, mono /= ""]
                  monomyal = createMonomyal strings
                  strings = removeSpaces s
                  s = splitString x




sortLitAlg :: (Char,Int) -> (Char,Int) -> Ordering
sortLitAlg lit1 lit2 | snd lit1 < snd lit2 = LT
                     | snd lit1 > snd lit2 = GT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 < fst lit2)) = GT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 >= fst lit2)) = LT

addExpo :: [(Char,Int)] -> [(Char,Int)]
addExpo (x:xs) = [(fst x, sum ([snd y | y <- xs, (fst x) == (fst y)] ++ [snd x]))] ++ addExpo [z | z <- xs, (fst x) /= (fst z)]
addExpo [] = []



sortLiteral :: [(Char,Int)] -> [(Char,Int)]
sortLiteral l = reverse (sortBy sortLitAlg expoAdded)
                where expoAdded = addExpo l

sortFactor :: Factor -> Factor -> Ordering
sortFactor fac1 fac2 |  degree1 < degree2 = LT
                     |  degree1 > degree2 = GT
                     | (degree1 == degree2) && (lit1 < lit2) = GT
                     | (degree1 == degree2) && (lit1 > lit2) = LT
                     | (degree1 == degree2) && (lit1 == lit2) && (length ((snd fac1)) > length ((snd fac2))) = GT
                     | (degree1 == degree2) && (lit1 == lit2) && (length ((snd fac1)) <= length ((snd fac2))) = LT

                     where degree1 = snd ( head (snd fac1))
                           degree2 = snd ( head (snd fac2))
                           lit1 = fst ( head (snd fac1))
                           lit2 = fst ( head (snd fac2))

sortPoli :: [Factor] -> [Factor]
sortPoli l = reverse(sortBy sortFactor [(fst x, sortLiteral(snd x)) | x <- l, fst x /= 0])



addFactor :: [Factor] -> [Factor]
addFactor (x:xs) = [(sum([fst y | y <- xs, (snd x) == (snd y)] ++ [fst x]) ,snd x)] ++ addFactor [z | z <- xs, (snd x) /= (snd z)]
addFactor [] = []


normalize :: Poli -> Poli
normalize polinom = [eliminate0Degree y | y<- l2]
                    where l1 = sortPoli ([(fst z, [(' ', 0)]) | z <- polinom, (snd z) == []] ++ [z | z <- polinom, (snd z) /= []])
                          l2 = addFactor l1

normalizeString :: String -> String
normalizeString spoli =  outputString (normalize (createPoly spoli))




addTwoPolis :: Poli -> Poli -> Poli
addTwoPolis pol1 pol2 = normalize (pol1 ++ pol2)

addTwoPolisString :: String -> String -> String
addTwoPolisString pol1 pol2 = outputString (addTwoPolis (createPoly pol1) (createPoly pol2))



multiplicatePolis :: Poli -> Poli -> Poli
multiplicatePolis pol1 pol2 = normalize [((fst x) * (fst y), (snd x) ++ (snd y)) | x <- pol3, y <- pol4]
                  where pol3 = normalize pol1
                        pol4 = normalize pol2



multiplicatePolisString :: String -> String -> String
multiplicatePolisString pol1 pol2 = outputString (multiplicatePolis (createPoly pol1) (createPoly pol2))




multiplicateCoef :: Poli -> Char -> Poli
multiplicateCoef (x:xs) var = [(product([fst x] ++ [snd y | y <- (snd x), (fst y) == var]), snd x)] ++ multiplicateCoef xs var
multiplicateCoef [] var = []


reduceDegree :: Factor -> Char -> Factor
reduceDegree fac1 var = (fst fac1, [(fst x, snd x - 1) | x <- snd fac1, (fst x) == var] ++ [x | x <- snd fac1, (fst x) /= var])


eliminate0Degree :: Factor -> Factor
eliminate0Degree fac1 = (fst fac1, [(' ', 0) | x <- snd fac1, ((snd x) == 0) && length(snd fac1) == 1] ++ [x | x<- snd fac1, (snd x) /= 0])


eliminateTerms :: Factor -> Char -> Bool
eliminateTerms x var = foldr (||) False [var == (fst y)| y <- (snd x)]


derivatePoli :: Poli -> Char -> Poli
derivatePoli pol1 var = normalize [eliminate0Degree x | x <- pol5]
                        where pol5 = [x | x <- pol4, eliminateTerms x var]
                              pol4 = normalize [reduceDegree x var | x <- pol3]
                              pol3 = multiplicateCoef pol2 var
                              pol2 = normalize pol1


derivatePoliString :: String -> Char -> String
derivatePoliString pol1 var = outputString (derivatePoli (createPoly pol1) var)




showLiteral :: [(Char,Int)] -> String
showLiteral (x:xs) | (fst x) == ' ' = ""
                   | (snd x) == 1 = [fst x] ++ showLiteral xs
                   | otherwise = [fst x] ++ "^" ++ show (snd x) ++ showLiteral xs
showLiteral [] = []




createString :: Poli -> String
createString (x:xs) |(fst x) > 0 = "+" ++ show (fst x) ++ showLiteral (snd x) ++ createString xs
                    |otherwise = show (fst x) ++ showLiteral (snd x) ++ createString xs
createString [] = ""


outputString :: Poli -> String
outputString pol1 | (head pol2) == '+' = drop 1 pol2
                  | otherwise = pol2
                  where pol2 = createString pol1

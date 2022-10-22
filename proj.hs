module Proj where

import Data.List
import Data.Ord
import Data.Char
import System.IO
import Data.String



type Factor = (Int, [(Char,Int)])
type Poli = [Factor]


-- ################################################ NORMALIZAÇÃO ########################################################

--Função que recebe um polinomio e retorna o mesmo normalizado
normalize :: Poli -> Poli
normalize polinom = [eliminate0Degree y | y<- l2]
                    where l1 = sortPoli ([(fst z, [(' ', 0)]) | z <- polinom, (snd z) == []] ++ [z | z <- polinom, (snd z) /= []])
                          l2 = addFactor l1

--Função que recebe uma string e retorna a normalização da mesma em string
normalizeString :: String -> String
normalizeString spoli =  outputString (normalize (createPoly spoli))


--Função de comparação de dois literais para ordenação por grau e literal
sortLitAlg :: (Char,Int) -> (Char,Int) -> Ordering
sortLitAlg lit1 lit2 | snd lit1 < snd lit2 = LT
                     | snd lit1 > snd lit2 = GT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 < fst lit2)) = GT
                     | ((snd lit1 == snd lit2) &&  (fst lit1 >= fst lit2)) = LT

--Função que recebe uma lista de literais e retorna a mesma com os expoentes dos literais com o mesmo caracter somados
addExpo :: [(Char,Int)] -> [(Char,Int)]
addExpo (x:xs) = [(fst x, sum ([snd y | y <- xs, (fst x) == (fst y)] ++ [snd x]))] ++ addExpo [z | z <- xs, (fst x) /= (fst z)]
addExpo [] = []


--Função que ordena a lista de literais de um fator por grau e literal
sortLiteral :: [(Char,Int)] -> [(Char,Int)]
sortLiteral l = reverse (sortBy sortLitAlg expoAdded)
                where expoAdded = addExpo l

--Função de comparação de dois Fatores para ordenação por grau e literal
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

--Função que ordena uma lista de fatores por grau e, consequentemente, por literal
sortPoli :: [Factor] -> [Factor]
sortPoli l = reverse(sortBy sortFactor [(fst x, sortLiteral(snd x)) | x <- l, fst x /= 0])


--Função que recebe uma lista de fatores e adiciona os coeficientes de todos aqueles que tem o mesmo literal
addFactor :: [Factor] -> [Factor]
addFactor (x:xs) = [(sum([fst y | y <- xs, (snd x) == (snd y)] ++ [fst x]) ,snd x)] ++ addFactor [z | z <- xs, (snd x) /= (snd z)]
addFactor [] = []


-- ######################################################################################################################

-- ################################################ ADIÇÃO ##############################################################

--Função que recebe dois polinomios e retorna a adição dos mesmos em polinomio
addTwoPolis :: Poli -> Poli -> Poli
addTwoPolis pol1 pol2 = normalize (pol1 ++ pol2)

--Função que recebe duas string e retorna a adição das mesmas em string
addTwoPolisString :: String -> String -> String
addTwoPolisString pol1 pol2 = outputString (addTwoPolis (createPoly pol1) (createPoly pol2))

-- ######################################################################################################################

-- ################################################ MULTIPLICAÇÃO #######################################################

--Função que recebe dois polinomios e retorna a multiplicação dos mesmos em polinomio
multiplicatePolis :: Poli -> Poli -> Poli
multiplicatePolis pol1 pol2 = normalize [((fst x) * (fst y), (snd x) ++ (snd y)) | x <- pol3, y <- pol4]
                  where pol3 = normalize pol1
                        pol4 = normalize pol2


--Função que recebe duas string e retorna a multiplicação das mesmas em string
multiplicatePolisString :: String -> String -> String
multiplicatePolisString pol1 pol2 = outputString (multiplicatePolis (createPoly pol1) (createPoly pol2))

-- ######################################################################################################################

-- ################################################ DERIVAÇÃO ###########################################################

--Função que recebe um polinomio e um literal e retorna a sua derivada em polinomio
derivatePoli :: Poli -> Char -> Poli
derivatePoli pol1 var = normalize [eliminate0Degree x | x <- pol5]
                        where pol5 = [x | x <- pol4, eliminateTerms x var]
                              pol4 = normalize [reduceDegree x var | x <- pol3]
                              pol3 = multiplicateCoef pol2 var
                              pol2 = normalize pol1

--Função que recebe uma string e um literal e retorna a derivada em string
derivatePoliString :: String -> Char -> String
derivatePoliString pol1 var = outputString (derivatePoli (createPoly pol1) var)

--Função utilizada na derivação que multiplica o coeficiente pelo expoente do literal pelo qual estamos a derivar.
multiplicateCoef :: Poli -> Char -> Poli
multiplicateCoef (x:xs) var = [(product([fst x] ++ [snd y | y <- (snd x), (fst y) == var]), snd x)] ++ multiplicateCoef xs var
multiplicateCoef [] var = []

--Função utilizada na derivação que reduz em 1 o expoente do literal igual ao caracter pelo qual estamos a derivar.
reduceDegree :: Factor -> Char -> Factor
reduceDegree fac1 var = (fst fac1, [(fst x, snd x - 1) | x <- snd fac1, (fst x) == var] ++ [x | x <- snd fac1, (fst x) /= var])

--Função utilizada na derivação que substitui o caracter do literal por um espaço para todos os literais de grau zero.
eliminate0Degree :: Factor -> Factor
eliminate0Degree fac1 = (fst fac1, [(' ', 0) | x <- snd fac1, ((snd x) == 0) && length(snd fac1) == 1] ++ [x | x<- snd fac1, (snd x) /= 0])

--Função utilizada na derivação que retorna True em caso de encontrar uma ocorrencia do literal pelo qual estamos a derivar.
eliminateTerms :: Factor -> Char -> Bool
eliminateTerms x var = foldr (||) False [var == (fst y)| y <- (snd x)]

-- ####################################################################################################################

-- ################################################ POLI -> STRING ####################################################

--Função que executa a conversão de um polinomio numa string
createString :: Poli -> String
createString (x:xs) |(fst x) > 0 = "+" ++ show (fst x) ++ showLiteral (snd x) ++ createString xs
                    |otherwise = show (fst x) ++ showLiteral (snd x) ++ createString xs
createString [] = ""

--Função que chama a função de conversão do polinomio e retira o caracter '+' no inicio do mesmo
outputString :: Poli -> String
outputString pol1 | (head pol2) == '+' = drop 1 pol2
                  | otherwise = pol2
                  where pol2 = createString pol1

--Função que recebe a parte literal de um polinomio e converte-a numa string
showLiteral :: [(Char,Int)] -> String
showLiteral (x:xs) | (fst x) == ' ' = ""
                   | (snd x) == 1 = [fst x] ++ showLiteral xs
                   | otherwise = [fst x] ++ "^" ++ show (snd x) ++ showLiteral xs
showLiteral [] = []

-- ####################################################################################################################


-- ################################################ STRING -> POLI ####################################################

--Função que recebe uma String e retorna o polinomio que resulta da sua conversão
createPoly :: String -> Poli
createPoly x = [(fst z, [(' ', 0)]) | z <- poly, (snd z) == []] ++ [z | z <- poly, (snd z) /= []]
            where poly = [takeCoef mono | mono <- monomyal, mono /= ""]
                  monomyal = createMonomyal strings
                  strings = removeSpaces s
                  s = splitString x

--Função que separa uma string pelos "+" e "-"
splitString :: String -> [String]
splitString poly | (take 1 poly) == "-" = [take 1 poly] ++ splitString (drop 1 poly)
                 | otherwise = case dropWhile (\x -> x == '+' || x == '-') poly of
                                    "" -> []
                                    s' -> w : splitString s''
                                        where (w, s'') = break (\x -> x =='+' || x =='-') s'

--Função que remove todos os espaços de uma string
removeSpaces :: [String] -> [String]
removeSpaces x = [filter (\xs -> (xs /=' ')) y | y <- x]

--Função que retira os simbolos positivos e adiciona o simbolo negativo à string seguinte da lista retornando uma lista de monomios
createMonomyal :: [String] -> [String]
createMonomyal (x:xs:xss) |x == "-" = [x ++ xs] ++ createMonomyal xss
                          |x == "+" = [xs] ++ createMonomyal xss
                          |otherwise = [x] ++ createMonomyal ([xs] ++ xss)
createMonomyal (x:[]) = [x]
createMonomyal [] = []

--Função que recebe uma string e converte-a num literal
createLiteral :: String -> [(Char, Int)]
createLiteral (x:xs:xss) | isLetter xs = [(x, 1)] ++ createLiteral ([xs] ++ xss)
                         | xs == '^' = [(x, read (takeWhile (isDigit) xss) :: Int)] ++ createLiteral (dropWhile (isDigit) xss)
createLiteral (x:[]) = [(x, 1)]
createLiteral [] = []


-- ???
takeCoef :: String -> Factor
takeCoef (x:xs)
  | isDigit x = ((read ([x] ++ (takeWhile (isDigit) xs)) :: Int), createLiteral (dropWhile (isDigit) xs))
  | x == '-' = if isDigit (head xs) then ((read ([x] ++ (takeWhile (isDigit) xs)) :: Int), createLiteral (dropWhile (isDigit) xs))
              else (-1, createLiteral xs)
  | isLetter x = (1, createLiteral ([x] ++ xs))

-- ######################################################################################################################

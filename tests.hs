import Proj
import Test.QuickCheck

genFactor :: Gen Factor 
genFactor = 
  do 
    coef <- arbitrary
    literal <- arbitrary
    degree <- arbitrary
    return (Factor (coef,[(literal,degree)]))

prop_associativity_sum :: Poli -> Poli -> Bool
prop_associativity_sum p1 p2 = addTwoPolis p1 p2 == addTwoPolis p2 p1

main :: IO()
main = do
    putStrLn "\n####### Function Tests #######\n"
    quickCheck (withMaxSuccess 100 prop_associativity_sum)


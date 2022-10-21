import Proj
import Test.QuickCheck

prop_associativity_sum :: Poli -> Poli -> Bool
prop_associativity_sum p1 p2 = addTwoPolis p1 p2 == addTwoPolis p2 p1

instance Arbitrary Factor where
   arbitrary = do
     x <- arbitrary
     y <- arbitrary
     z <- arbitrary
     return $ (x, [(y,z)])

main :: IO()
main = do
    putStrLn "\n####### Function Tests #######\n"
    quickCheck (withMaxSuccess 100 prop_associativity_sum)

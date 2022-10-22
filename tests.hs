import Proj
import Test.QuickCheck



prop_associativity_sum :: Poli -> Poli -> Bool
prop_associativity_sum p1 p2 = addTwoPolis p1 p2 == addTwoPolis p2 p1

prop_null_element_sum :: Poli -> Bool
prop_null_element_sum p1 = addTwoPolis p1 [(0,[(' ', 0)])] == normalize p1

prop_associativity_mult :: Poli -> Poli -> Bool
prop_associativity_mult p1 p2 = multiplicatePolis p1 p2 == multiplicatePolis p2 p1
{-
prop_null_element_mult :: Poly -> Bool
prop_null_element_mult p1 = mult p1 (1,[]) == p1

prop_coef_deriv :: Poly -> Bool
prop_coef_deriv p1 = deriv (mult p1 (5,[])) == mult (5,[]) (deriv p1)

prop_null_elem_deriv :: Poly -> Bool
prop_null_elem_deriv = deriv ( p1 :: [(5,[])]) ==  deriv p1

prop_sum_deriv :: Poly -> Poly -> Bool
prop_sum_deriv p1 p2 = deriv (sum p1 p2) == sum (deriv p1) (deriv p2)

-}


main :: IO()
main = do
    putStrLn "\n####### Function Tests #######\n"
    --quickCheck (withMaxSuccess 100 prop_associativity_sum)
    --quickCheck (withMaxSuccess 100 prop_null_element_sum)
    quickCheck (withMaxSuccess 30 prop_associativity_mult)  

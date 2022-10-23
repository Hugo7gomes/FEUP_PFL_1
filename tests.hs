import Proj

main :: IO()
main = do
    putStrLn "\n####### Normalize a Polynomial #######\n"
    putStrLn "Case 1: "
    putStrLn "10z + 5xy^2"
    putStrLn "Result: "
    putStrLn (normalizeString "10z + 5xy^2")
    putStrLn "\nCase 2: "
    putStrLn "3"
    putStrLn "Result: "
    putStrLn (normalizeString "3")
    putStrLn "\nCase 3: "
    putStrLn "10z + 5xy^2+ 4z"
    putStrLn "Result: "
    putStrLn (normalizeString "10z + 5xy^2+ 4z")
    putStrLn "\nCase 4: "
    putStrLn ""
    putStrLn "Result: "
    putStrLn (normalizeString "")
    putStrLn "\nCase 5: "
    putStrLn "5x^2 -5x^2"
    putStrLn "Result: "
    putStrLn (normalizeString "5x^2 -5x^2")
    putStrLn "\nCase 6: "
    putStrLn "5x^2x + 3x^2"
    putStrLn "Result: "
    putStrLn (normalizeString "5x^2x + 3x^2")

    putStrLn "\n####### Sum Two Polynomials #######\n"
    putStrLn "Case 1: "
    putStrLn "10z + 5xy^2"
    putStrLn "0"
    putStrLn "Result: "
    putStrLn (addTwoPolisString "10z + 5xy^2" "0")
    putStrLn "\nCase 2: "
    putStrLn "10z + 5xy^2"
    putStrLn "15z + 3x"
    putStrLn "Result: "
    putStrLn (addTwoPolisString "10z + 5xy^2" "15z + 3x")

    putStrLn "\n####### Multiplicate Two Polynomials #######\n"
    putStrLn "Case 1: "
    putStrLn "10z + 5xy^2"
    putStrLn "0"
    putStrLn "Result: "
    putStrLn (multiplicatePolisString "10z + 5xy^2" "0")
    putStrLn "\nCase 2: "
    putStrLn "10z + 5xy^2"
    putStrLn "15z + 3x"
    putStrLn "Result: "
    putStrLn (multiplicatePolisString "10z + 5xy^2" "15z + 3x")
    putStrLn "\nCase 3: "
    putStrLn "10z + 5xy^2"
    putStrLn "1"
    putStrLn "Result: "
    putStrLn (multiplicatePolisString "10z + 5xy^2" "1")


    putStrLn "\n####### Derivate a Polynomial #######\n"
    putStrLn "Case 1: "
    putStrLn "10z + 5xy^2"
    putStrLn "x"
    putStrLn "Result: "
    putStrLn (derivatePoliString "10z + 5xy^2" 'x')
    putStrLn "\nCase 2: "
    putStrLn "10x + 5xy^2"
    putStrLn "z"
    putStrLn "Result: "
    putStrLn (derivatePoliString "10x + 5xy^2" 'z')
    putStrLn "\nCase 3: "
    putStrLn "10x^2 + 5xy^2"
    putStrLn "x"
    putStrLn "Result: "
    putStrLn (derivatePoliString "10x^2 + 5xy^2" 'x')

    putStrLn "\n####### String to Internal Representation #######\n"
    putStrLn "Case 1: "
    putStrLn "10z + 5xy^2"
    putStrLn "Result: "
    putStrLn $ show (createPoly "10z + 5xy^2")
    putStrLn "\nCase 2: "
    putStrLn "10"
    putStrLn "Result: "
    putStrLn $ show (createPoly "10")

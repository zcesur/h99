import Test.HUnit (assertEqual, runTestTT, Test(..))
import Data.List (intercalate)

-- Problem 46

-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
-- logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and
-- B succeed.

-- A logical expression in two variables can then be written as in the
-- following example: and(or(A,B),nand(A,B)).

-- Now, write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.

not' :: Bool -> Bool
and', or', nand', nor', xor', impl', equ' :: Bool -> Bool -> Bool

not' True = False
not' False = True

and' True True = True
and' _    _    = False

or' False False = False
or' _     _     = True

equ' True  True  = True
equ' False False = True
equ' _     _     = False

nand' x y = not' $ and' x y

nor' x y = not' $ or' x y

xor' x y = not' $ equ' x y

impl' True False = False
impl' _    _     = True

tablePure :: (Bool -> Bool -> Bool) -> String
tablePure f = 
    unlines $
    map (\(x,y) -> intercalate " " [show x, show y, show (f x y)]) $
    [ (x,y) | x <- [True, False], y <- [True, False] ]

table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ tablePure f

test1 = TestCase $ assertEqual "Problem 46" expected actual
  where
    expected = unlines ["True True True"
                       ,"True False True"
                       ,"False True False"
                       ,"False False False"]
    actual = tablePure (\a b -> (and' a (or' a b)))

main = do
    let tests = TestList
            [ test1 ]

    runTestTT tests 

import Test.HUnit (assertEqual, runTestTT, Test(..))
import Data.List (intercalate)

-- Problem 46
--
-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
-- logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and
-- B succeed.
--
-- A logical expression in two variables can then be written as in the
-- following example: and(or(A,B),nand(A,B)).
--
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

-- Problem 47
--
-- Truth tables for logical expressions (2).
--
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This
-- allows to write the logical expression in the more natural way, as in the
-- example: A and (A or not B). Define operator precedence as usual; i.e. as in
-- Java.

-- I determined the fixity of and', or' & equ' by looking at those of (&&),
-- (||) and (==) found in Haskell. As for the remaining operators, I found out
-- about their canonical definitions using the former 3 primitives, and
-- assigned fixities accordingly. For instance, impl' a b = or' (not' a) b
-- so I decided to let impl' have a fixity of 2.

infixr 3 `and'`
infixr 2 `or'`
infix 4 `equ'`
infixr 3 `nand'`
infixr 2 `nor'`
infix 4 `xor'`
infixr 2 `impl'`

test2 = TestCase $ assertEqual "Problem 47" expected actual
  where
    expected = unlines ["True True True"
                       ,"True False True"
                       ,"False True False"
                       ,"False False False"]
    actual = tablePure (\a b -> a `and'` (a `or'` not' b))

-- Problem 48

-- Truth tables for logical expressions (3).

-- Generalize problem P47 in such a way that the logical expression may contain
-- any number of logical variables. Define table/2 in a way that
-- table(List,Expr) prints the truth table for the expression Expr, which
-- contains the logical variables enumerated in List.

tablePure' :: Int -> ([Bool] -> Bool) -> String
tablePure' n f = 
    unlines $
    map (\x -> intercalate " " (map show (x ++ [f x]))) $
    sequence $ replicate n [True, False]
     
table' :: Int -> ([Bool] -> Bool) -> IO ()
table' n f = putStrLn $ tablePure' n f

test3 = TestCase $ assertEqual "Problem 48" expected actual
  where
    expected = unlines ["True True True True"
                       ,"True True False True"
                       ,"True False True True"
                       ,"True False False False"
                       ,"False True True False"
                       ,"False True False False"
                       ,"False False True False"
                       ,"False False False False"]
    actual = tablePure' 3 (\[a,b,c] ->
        a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)

-- Problem 49

-- Gray codes.

-- An n-bit Gray code is a sequence of n-bit strings constructed according to
-- certain rules. For example,

-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].

-- Find out the construction rules and write a predicate with the following
-- specification:

-- % gray(N,C) :- C is the N-bit Gray code

-- Can you apply the method of "result caching" in order to make the predicate
-- more efficient, when it is to be used repeatedly?

gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = let xs = gray (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)

test4 = TestCase $ assertEqual "Problem 49" expected actual
  where
    expected = ["000","001","011","010","110","111","101","100"]
    actual = gray 3

main = do
    let tests = TestList
            [ test1
            , test2
            , test3
            , test4 ]

    runTestTT tests 

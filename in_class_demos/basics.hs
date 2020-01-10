module Basics where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :help, :load, :reload, :quit, :type, :info
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions


---------------------
-- Basic Functions --
---------------------

-- * function types
-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
double :: Int -> Int
double x = 2 * x


-- | Is this integer zero?
isZero :: Int -> Bool

--Option 1:
isZero 0 = True
isZero _ = False

--Option 2:
--isZero x = x == 0


-- | Is this integer non-zero?
isNonZero :: Int -> Bool

--Option 1:
--isNonZero x = not (isZero x)

--Option 2:
--isNonZero x = x /= 0

--Option 3:
--isNonZero 0 = False
--isNonZero _ = True

--Option 4 (using function composition, i.e. '.' is like composition in math):
isNonZero = not . isZero


-- | Computes the average of two floating point numbers.
avg :: Float -> Float -> Float
avg x_1 x_2 = (x_1 + x_2) / 2


-- | Uses avg to compute half of a floating point number.
half :: Float -> Float
half x = avg x 0



-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
-- * anonymous functions


----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name
--   * a set of cases, each with:
--     * a data constructor
--     * zero or more arguments
-- * more pattern matching
--   * top-level and case-expressions

-- | An example data type with two cases.
data Result = OK Int | Error
  deriving (Eq,Show)

-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv _ 0 = Error
safeDiv x y = OK (x `div` y)

-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
--fromResult (OK x) = x
--fromResult Error 
fromResult r =  case r of
  Error -> 0
  OK x  -> x


-- | Add two results.
addResults :: Result -> Result -> Result
addResults (OK x) (OK y) = OK (x + y)
addResults _      _      = Error


-- The definition of Bool in the Haskell Prelude looks like this:
--   
--   data Bool = False | True


-- The type Result is similar to the Maybe type in the Prelude:
--
--   data Maybe a = Just a | Nothing



---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List
   = Nil
   | Cons Int List
  deriving (Eq,Show)

-- | The empty list.
empty :: List
empty = Nil

-- | The list: [2,3,4]
exList :: List
exList = Cons 2 (Cons 3 (Cons 4 (Nil)))

-- | Compute the length of a list.
listLength :: List -> Int
listLength Nil        = 0
listLength (Cons _ t) = 1 + listLength t

-- | Compute the sum of the integers in a list.
listSum :: List -> Int
listSum Nil        = 0
listSum (Cons h t) = h + listSum t


-- Example evaluation:
--
-- listSum (Cons 3 (Cons 4 Nil))
-- => 3 + listSum (Cons 4 Nil)
-- => 3 + (4 + listSum (Nil))
-- => 3 + (4 + 0)
-- => 7


-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [a]
--    = []         -- Nil
--    | a : [a]    -- Cons

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
length = undefined

-- | Compute the sum of an integer list.
sum :: [Int] -> Int
sum = undefined

-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product = undefined

allOdd :: [Int] -> Bool
allOdd = undefined

-- | Double all the elements in an integer list.
doubleAll :: [Int] -> [Int]
doubleAll = undefined

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll = undefined

-- | Apply the even function to all elements in the list.
evenAll :: [Int] -> [Bool]
evenAll = undefined


----------------------------
-- Higher-Order Functions --
----------------------------

-- * map and foldr


-- | Map a function over the elements in a list.
map = undefined

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = undefined

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = undefined

-- | Reimplement evenAll using map.
evenAll' :: [Int] -> [Bool]
evenAll' = undefined

-- | Fold an accumulator function over the elements in a list.
foldr = undefined

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = undefined

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = undefined

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = undefined

-- | Reimplement allOdd using foldr.
allOdd' :: [Int] -> Bool
allOdd' = undefined

-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
countTrues = undefined

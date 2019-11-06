module Test where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import qualified Control.Monad as M
import qualified Control.Applicative as A
import Control.Monad.Writer as W

test :: String -> String
test x = "."

head' :: [a] -> a
head' [] = error "empty"
head' (a:_) = a

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

strMod1 :: String -> String
strMod1 "" = ""
strMod1 all@(x:xs) = all ++ "    " ++ strMod1 xs

bmiCalc :: (RealFloat a) => a -> a -> String
bmiCalc w h
    | bmi <= uw           = "underweight"
    | bmi <= n            = "normal"
    | otherwise           = "overweight"
    where bmi = w / (h ^ 2)
          (uw, n) = (18.5, 25.0)

min' :: (Ord a) => a -> a -> a
min' a b
    | a < b     = a
    | otherwise = b

getInitials :: String -> String -> String
getInitials f l = [x] ++ " " ++ [y]
    where x:_ = f
          y:_ = l

bmisCalc :: (RealFloat a) => [(a, a)] -> [a] 
bmisCalc xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi <= 1000.0]

calcRectangleSurfaceArea :: (Num a) => a -> a -> a -> a
calcRectangleSurfaceArea l w h =
    let a1 = l * w
        a2 = w * h
        a3 = l * h
    in 2 * (a1 + a2 + a3)

tupleMod1 :: (RealFloat a) => (a, a, a) -> a
tupleMod1 t = let (x, y, z) = t in (x+y+z)/3

tupleMod2 :: (RealFloat a) => (a, a, a) -> (a, a, a)
tupleMod2 t = (avg, avg, avg)
    where avg = let (x, y, z) = t in (x+y+z)/3

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "singleton"
                                               xs -> "not empty or singleton"

describeNumModThree :: (Integral a) => a -> String
describeNumModThree x = "the number is" ++ check x ++ " divisible by 3"
    where check x
            | x `mod` 3 == 0 = ""
            | otherwise      = " not"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

fibonacci' :: (Num a, Ord a) => a -> a
fibonacci' x
    | x < 0     = error "negative number"
    | x == 1    = 0
    | x == 2    = 1 
    | otherwise = fibonacci' (x-1) + fibonacci' (x-2)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let s = quicksort' [a | a <- xs, a <= x]
        b = quicksort' [a | a <- xs, a > x]
    in s ++ [x] ++ b

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

replicateCurriedThree :: a -> [a]
replicateCurriedThree = replicate' 3

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

-- partApp2 and partApp3 are equiv., 3 is then irrelev.
-- a partially applied fn does not have to be returned
-- since partApp1 takes an anon. partial fn as an arg
-- so partApp2 eval'd w. 1 arg is sufficient (ghci
-- returns the partial fn by def.)
-- i guess the def'n of partApp3 makes it more
-- explicit that partApp2 should have the first arg
-- partially applied and then subsequently have
-- the second arg partially applied

partApp1 :: (Integral a) => (a -> a) -> a -> a
partApp1 f = f

partApp2 :: (Integral a) => a -> a -> a
partApp2 x y = (x `mod` y) `mod` 2

partApp3 :: (Integral a) => a -> (a -> a)
partApp3 = partApp2

flip':: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map'  _ [] = []
map' f (x:xs) = f x:map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x:filter' f xs
    | otherwise = filter' f xs

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) =
    let s = quicksort' (filter' (<=x) xs)
        b = quicksort' (filter' (>x) xs)
    in s ++ [x] ++ b

largestDiv10000 :: (Integral a) => a -> a
largestDiv10000 n = head (filter' f [10000, 9999 ..])
                        where f x = x `mod` n == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) 
    | f x       = x:takeWhile' f xs
    | otherwise = []

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain a 
    | even a    = a:collatzChain (a `div` 2)       
    | otherwise = a:collatzChain (3 * a + 1)

numChainsGT :: Int -> Int -> Int
numChainsGT 0 _ = 0
numChainsGT s 0 = s
numChainsGT s c 
    | length (collatzChain s) > c = 1 + numChainsGT (s - 1) c
    | otherwise                   = numChainsGT (s - 1) c

(-:) :: a -> (a -> b) -> b
x -: f = f x  

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x = foldl (\acc i -> (i == x) || acc) False

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x:acc) []

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldr1 (\x acc -> if x > acc then x else acc)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []

div' :: Int -> Int -> Bool
div' x y 
    | y `mod` x == 0 = True
    | otherwise      = False

-- "the prefered style is to use let bindings to give labels 
-- to intermediary results or split the problem into sub-problems 
-- and then put it together so that the function makes sense to 
-- a reader, rather than making a huge composition chain"

oddSqSum :: Int
oddSqSum = sum . takeWhile (<1000) . filter odd . map (^2) $ [1,3..]

intersperseZero :: (Num a) => [a] -> [a]
intersperseZero = intersperse 0

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

removeSpecialChars :: String -> String
removeSpecialChars [] = []
removeSpecialChars (x:xs)
    | allowed x = x:removeSpecialChars xs
    | otherwise = removeSpecialChars xs
        where allowed s = s `elem` ['A'..'Z'] || 
                          s `elem` ['a'..'z'] ||
                          s `elem` ['0', '1', '2', '3', '4', '5'] ||
                          s `elem` ['6', '7', '8', '9', '-', '='] ||
                          s `elem` ['.', ',', '/', '\\', '\'', ';', ':'] ||
                          s `elem` ['{', '}', '!', '@', '#', '$', '%', '+'] ||
                          s `elem` ['(', ')', '|', '[', ']', '`', '~', '_'] ||
                          s `elem` ['?', ' ', '^', '&', '*', '<', '>', '\"']

-- intersperse: a -> [a] -> [a] (place elem among list)
-- intercalate: [a] -> [[a]] -> [a] (place list among list of lists)
-- transpose: [[a]] -> [[a]] (matrix transpose)
-- foldl', foldr', foldl1', foldr1' (strict eval, accumulator evals
--          intermediate vals as its traversing the list)
-- concat: [[a]] -> [a] (flattens 2d list)
-- concatMap: (a -> [b]) -> [a] -> [b] (maps fn to 2d list)
-- iterate: (a -> a) -> a -> a (applies fn to starting val,
--          then the result, then the result of that, etc etc)
-- splitAt: Int -> [a] -> ([a], [a]) (splits list at some number,
--          returns tuple of lists at the split loc)
-- takeWhile, dropWhile: returns list of all elems that satisfy predicate,
--          (ie until first False eval). dropWhile does opposite, returning
--          returning all elems on other side of first False eval
-- span: (a-> Bool) -> [a] -> ([a], [a]) (like splitAt but 
--          takes predicate rather than number)
-- break: span (not predicate) xs
-- sort: sorts a list
-- group: groups adjacent elements into sublists if they are equal
-- map (\l@(x:xs) -> (x,length l)) . group . sort $ list
--          maps a lambda which takes a list and prods a tuple of the
--          first elem (in this case all elems in lambda list are the same)
--          and the lambda list length onto a sorted, grouped list. result:
--          returns a frequency count of each list elem
-- inits, tails: apply init/tail recurs. to a list until it is empty
-- isInfixOf: searches for a sublist within a list and 
--          returns True if the sublist we're looking 
--          for is somewhere inside the target list.
-- isPrefixOf, isSuffixOf: isInfixOf but checks beg/end of list for the sublist
-- partition: takes a list and a predicate and returns a tuple of lists.
--          first elem is a list of all True, second all False
-- find: takes a list and a predicate and returns the 
--          first element that satisfies the predicate (wrapped in a Maybe)
-- elemIndex: returns index of elem (wrapped in a Maybe) || Nothing
-- elemIndices: returns list of indices of elem
-- lines: splits strings by \n into list
-- unlines: inverse of lines
-- words/unwords: like un/lines but with spaces
-- nub: removes dupl elems in list
-- delete: removes first occurr of elem in list
-- \\ set difference
-- union: union
-- intersect: intersection
-- insert: takes an element and a list of elements that can be sorted
--          and inserts it into the last pos where it's leq than next elem
-- Data.List has their more generic equivalents: 
--          genericLength, genericTake, genericDrop, genericSplitAt, 
--          genericIndex and genericReplicate. takes any Integral param
-- The nub, delete, union, intersect and group functions all have 
--          their more general counterparts called 
--          nubBy, deleteBy, unionBy, intersectBy and groupBy.
--          take an equality fn as an extra param

-- f `on` g = \x y -> f (g x) (g y); used to define custom ordering/equality

findVal' :: (Eq k) => k -> [(k,v)] -> Maybe v
findVal' k = foldr (\(i, j) acc -> if i == k then Just j else acc) Nothing

-- Map.Map k v
-- Map.insert, null, size, singleton, lookup, member (`elem`), map, filter
-- Map.fromListWith, insertWith take fn params to operate on map keys/vals
-- eg Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
-- fromList [(2,108),(3,62),(4,37)]
-- eg Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]  
-- fromList [(3,104),(5,103),(6,339)]

-- Set.Set
-- Set.union, intersection, difference
-- Set.insert, null, size, singleton, lookup, member (`elem`), map, filter

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surfaceA :: Shape -> Float
surfaceA (Circle _ _ r) = pi * r ^ 2
surfaceA (Rectangle x1 y1 x2 y2) = abs $ (x2 - x1) * (y2 - y1)

-- `(..)` req. to get constructors from a type; eg `import ... , Shape(..), ...`
-- to get `Circle` & `Rectangle`

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surfaceA' :: Shape' -> Float
surfaceA' (Circle' _ r) = pi * r ^ 2
surfaceA' (Rectangle' (Point x1 y1) (Point x2 y2)) = abs $ (x2 - x1) * (y2 - y1)

shift :: Shape' -> Float -> Float -> Shape'
shift (Circle' (Point x y) r) a b = Circle' (Point (x + a) (y + b)) r
shift (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

data Person' = Person' { first :: String
                     , last :: String
                     , age' :: Int
                     , height :: Float
                     , phone :: String
                     , bloodtype :: Char
                     } deriving (Show)

data Vector' a = Vector' a a a deriving (Show)
vplus :: (Num a) => Vector' a -> Vector' a -> Vector' a
(Vector' i j k) `vplus` (Vector' l m n) = Vector' (i + l) (j + m) (k + n)

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

-- GHC will see if the value constructors match, then it will see if the
-- types of all the fields also have to be part of the Eq typeclass for GHC
-- to automatically define `==` and `\=` for a typeclass
-- for Show/Read, all fields also need to be part of Show/Read
-- this is because when GHC defines these fns automagically, it does so
-- by iterating thru the fields of the typeclass

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Eq: ==, \=
-- Ord: GT, LT
-- Show: repr data at Str
-- Read: take Str and repr it as type `typeclass`
-- Bounded: minBound, maxBound defn'd
-- Enum: can take vals in typeclass and create list range, defn succ, pred
-- `type` keyword: used for making a synonym for an already existing type

type Day' = Day

type PhoneNumber = String  
type Name' = String  
type PhoneBook = [(Name',PhoneNumber)]    -- phoneBook :: [(String, String)]

-- can use lazy eval to partially apply type params to type constr.s
type IntMap = Map.Map Int    -- type IntMap v = Map Int v

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
-- takes a locker # (for key), and tuple (state, code)
type LockerMap = IntMap (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number lmap =
    case Map.lookup number lmap of
        Nothing            -> Left $ "locker #" ++ show number ++ " d.n.e"
        Just (state, code) -> if state /= Taken then Right code
                              else Left $ "locker #" ++ show number ++ " aleady taken"

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node n left right)
    | x > n     = Node n left (treeInsert x right)
    | x < n     = Node n (treeInsert x left) right
    | otherwise = Node x left right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node n left right)
    | x == n = True
    | x < n  = treeElem x left
    | x > n  = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- instance (Eq m) => Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False

-- class constraints in class declarations are used for making
-- a typeclass a subclass of another typeclass and class constraints 
-- in instance declarations are used to express requirements about 
-- the contents of some type

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

ifYesNo :: (YesNo a) => a -> b -> b -> b
ifYesNo cond yesVal noVal = if yesno cond then yesVal else noVal

data Vector'' a = Vector'' a a a

class (Eq a) => VectorOps a where
    vectormult :: Vector'' a -> Vector'' a -> Vector'' a

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node n left right) = Node (f n) (fmap f left) (fmap f right)

-- instance Functor (Either a) where  
--     fmap f (Right x) = Right (f x)  
--     fmap f (Left x) = Left x  
-- here, fmap :: (b -> c) -> Either a b -> Either a c
-- using partial application to make an instance of functor
-- Either a is a partially applied type, apply another type
-- to complete the constr.

-- instance Functor (Map.Map k) where
--     fmap f [()] = [()]
--     fmap f m = [(x, f y) | (x, y) <- toList m]

-- Functor's fmap only changes the type that is applied to the constr.

-- to get the value out of an I/O action, you have to perform it inside
-- another I/O action by binding it to a name with `<-`

-- print == putStrLn . show
-- sequence: evaluates list of mapped io fns, ie returns 
-- io actions from io fn calls in list
-- eg: sequence (map print [1,2,3,4,5]), sequence eval's the `print _`s
-- mapM somefn list == sequence (map somefn list)
-- mapM_ throws away results (useful for `print`s, not so much for `getLine`s)
-- when: like `while someCondition` for io actions
-- forever: like `while True` for io actions
-- forM: makes io action for every item in list, sort of like a for loop
-- interact: takes a (Str -> Str) param and uses it to parse + store input
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- takes a path to a file, an IOMode and a function that 
-- takes a handle and returns some I/O action 
-- returns is an I/O action that will open that file, 
-- do something we want with the file and then close it. 
-- ie a `with open(file, '_') as f` block in python.

getTemps :: [FilePath] -> [FilePath]
getTemps = foldr (\x acc -> if startsWithTemp x then x:acc else acc) []
                 where startsWithTemp x 
                        | take 4 x == "temp" = True
                        | otherwise          = False

printAndParseTemps :: IO[FilePath] -> IO()
printAndParseTemps dirc =  do
                            l <- dirc
                            print . getTemps $ l

-- newStdGen: splits our current random generator into two generators, 
-- updates the global random generator with one of them and 
-- encapsulates the other as its result

-- Data.ByteString: strict, whole thing eval'd on call
-- Data.ByteString.Lazy: divided into 64k chunks which are lazily eval'd.
-- each chunk is eval'd strictly, so it's like a list 
-- of 64k strict ByteStrings

copyFile' :: FilePath -> FilePath -> IO()
copyFile' s d = do
                 t <- B.readFile s
                 B.writeFile d t

data Stack a = EmptyStack | StackVal a (Stack a) deriving (Show, Read, Eq)

stackPush :: a -> Stack a -> Stack a
stackPush x EmptyStack = StackVal x EmptyStack
stackPush x (StackVal v s) = StackVal x (StackVal v s)

stackPop :: Stack a -> (Maybe a, Stack a)
stackPop EmptyStack = (Nothing, EmptyStack)
stackPop (StackVal v EmptyStack) = (Just v, EmptyStack)
stackPop (StackVal v (StackVal u s)) = (Just v, StackVal u s)

stackPeek :: Stack a -> Maybe a
stackPeek EmptyStack = Nothing
stackPeek (StackVal v s) = Just v

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
      where foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction xs numberString = read numberString:xs

-- can think of fmap as either a function that takes a function and a functor 
-- and then maps that function over the functor, or as a function that takes a 
-- function and lifts that function so that it operates on functors

-- instance Functor Stack where
--     fmap f (StackVal v s) = 

-- instance Applicative [] where
--     pure x = [x]
--     fs <*> xs = [f x | f <- fs, x <- xs]

-- applicative style: `pure f <*> x <*> y`, where f is a function and x, y are functors
-- or: f <$> x <*> y <*> ...

applicativeTest' :: (Num a) => [a] -> (a -> a -> a) -> [a] -> [a]
applicativeTest' xs f ys = f <$> xs <*> ys

-- <*> (unwraps a function from its functor to the default context and)
-- applies it to a functor

and' :: [Bool] -> Bool
and' = foldr (\x acc -> if not x then False else acc) True

-- x >> y = x >>= \_ -> y
-- (>>) returns the val on right unless the val on left is a failure
-- ie Nothing for Maybe, IOException in IO, [] for List, etc
-- thus do blocks are sequential ... since the last value is what is 
-- kept in context unless failure
-- (and if failure it gets passed all the way to
-- end as described above)

type KnightPos = (Int, Int)

getAvailMoves :: KnightPos -> [KnightPos]
getAvailMoves (x, y) = do
               (x', y') <- [ (x + 2, y - 1), (x + 2, y + 1) 
                           , (x - 2, y - 1), (x - 2, y + 1)
                           , (x + 1, y - 2), (x + 1, y + 2) 
                           , (x - 1, y - 2), (x - 1, y + 2)
                           ] 
               M.guard (x' `elem` [1..8] && y' `elem` [1..8])
               return (x', y')

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 k t = t `elem` (getAvailMoves k >>= getAvailMoves >>= getAvailMoves)

logNumber :: Int -> W.Writer [String] Int
logNumber x = writer (x, ["got num: " ++ show x])

multWithLog :: Int -> Int -> W.Writer [String] Int
multWithLog m n = do
                a <- logNumber m
                b <- logNumber n
                tell ["multiplied " ++ show m ++ " and " ++ show b ++ "."]
                return (a * b)

-- Writer 3 got3 {logNumber m} >>= (\x -> (Writer 5 got5) {logNumber n}) 
-- ==> Writer 5 got3, got5 {b}
-- Writer 3 got3 >>= (\x -> (Writer 5 got5)) ===> Writer 3 got3, got5 {a}
-- when the lambda (\x -> (Writer 5 got5)) is bound with b,
-- it modifies the "logs" of a as well.
-- since a and b share a monoid, the logs are all captured sequentially.
-- the 2 writer instances a and b have diff vals, but they share
-- a monoid (the "logs"), so 
-- however, when either a or b is returned, their "value" 
-- is specific to them
-- since the values aren't modified (like how "logs" are `mappend`ed)
-- because (eg a) Writer t logs >>= (\x -> Writer a []) ===> 
-- Writer a (logs ++ []) ===> Writer a logs
-- `do` notation represents a single monad

gcd' :: Int -> Int -> W.Writer [String] Int
gcd' a b
    | b == 0    = do
        tell ["finished with " ++ show a]
        return a
    | otherwise = do
        tell ["did " ++ show a ++ " mod " ++ show b]
        gcd' b (a `mod` b)

parabola' :: Int -> Int
parabola' = do
        a <- (^2)
        b <- (*2)
        return (a + b + 1)

keepSmall :: Int -> W.Writer [String] Bool
keepSmall a
    | a < 4    = do
        tell ["keeping " ++ show a]
        return True
    | otherwise = do
        tell ["dropping " ++ show a]
        return False

getPset :: [a] -> [[a]]
getPset = filterM (\x -> [True, False])

getAllCoinVals :: [(Bool, Double)] -> [(Bool, Double)]
getAllCoinVals xs = (True, getBool' (\(b, d) -> b) xs) :
                    [(False, getBool' (\(b, d) -> not b) xs)]
                where getBool' p xs = sum $ map snd $ filter p xs

freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' EmptyTree EmptyTree)
                (Node 'T' EmptyTree EmptyTree)
            )
            (Node 'Y'
                (Node 'S' EmptyTree EmptyTree)
                (Node 'A' EmptyTree EmptyTree)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' EmptyTree EmptyTree)
                (Node 'R' EmptyTree EmptyTree)
            )  
            (Node 'A'
                (Node 'A' EmptyTree EmptyTree)
                (Node 'C' EmptyTree EmptyTree)
            )
        )

data Direction = Left' | Right'

elemAt :: [Direction] -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (Left':ds) (Node _ l _) = elemAt ds l
elemAt (Right':ds) (Node _ _ r) = elemAt ds r

elemAtM :: (Show a) => [Direction] -> Tree a -> W.Writer [String] a
elemAtM [] (Node x _ _) = do
                        tell ["reached final pos: " ++ show x]
                        return x
elemAtM (Left':ds) (Node x l _) = do
                            tell ["turned left at: " ++ show x]
                            elemAtM ds l
elemAtM (Right':ds) (Node x _ r) = do
                            tell ["turned right at: " ++ show x]
                            elemAtM ds r

data Crumb a = LC a (Tree a) | RC a (Tree a) deriving Show

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, ds) = (l, LC x r:ds)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, ds) = (r, RC x l:ds)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (_, []) = (EmptyTree, [])
goUp (t, LC x r:ds) = (Node x t r, ds)
goUp (t, RC x l:ds) = (Node x l t, ds)

type Zipper a = (Tree a, Breadcrumbs a)

modify' :: (a -> a) -> Zipper a -> Zipper a
modify' f (EmptyTree, ds) = (EmptyTree, ds)
modify' f (Node x l r, ds) = (Node (f x) l r, ds)

treeAttach :: Tree a -> Zipper a -> Zipper a
treeAttach t (_, ds) = (t, ds)

goTop :: Zipper a -> Zipper a
goTop (t, []) = (t, [])
goTop z = goTop . goUp $ z

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b : bs) = (b : xs, bs)

type Name = String
type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "condiment.doc" "best mustard"
        , Folder "programs"
            [ File "wizard.exe" "10"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (i, FSCrumb n l r : bs) = (Folder n (l ++ [i] ++ r), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)
  
nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRename :: Name -> FSZipper -> FSZipper
fsRename n (Folder a xs, bs) = (Folder n xs, bs)
fsRename n (File a d, bs) = (File n d, bs)

goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, ds) = Just (l, LC x r:ds)
goLeft' (EmptyTree, _) = Nothing

goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Node x l r, ds) = Just (r, RC x l:ds)
goRight' (EmptyTree, _) = Nothing

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (_, []) = Nothing
goUp' (t, LC x r:ds) = Just (Node x t r, ds)
goUp' (t, RC x l:ds) = Just (Node x l t, ds)

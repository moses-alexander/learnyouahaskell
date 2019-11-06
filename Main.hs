module Main where


import Test
import Data.Char
import System.IO
import System.Directory
import Control.Monad
import Data.Monoid

main :: IO ()
main = do
    putStrLn "yo"
    print 0
    print (test "0")
    print (head' [4,3,2,1])
    print (head' "minimum")
    print (length' [1,2,45,77,4,8])
    print (strMod1 "moses")
    print (bmiCalc 117.0 68.0)
    print (min' 3 4)
    print (getInitials "moses" "alexander")
    print (bmisCalc [(117.0, 68.0), (127.0, 68.0),(137.0, 68.0)])
    print (calcRectangleSurfaceArea 2 3 4)
    print (tupleMod1 (1.0, 2.0, 3.0))
    print (tupleMod2 (4.0, 5.0, 6.0))
    print (describeList [])
    print (describeList [2])
    print (describeList [1, 2, 3])
    print (describeNumModThree 69)
    print (describeNumModThree 44)
    print (maximum' [2,3,4,23,45,22,11,99])
    print (replicate' 3 3)
    print (take' 3 [2,3,4,23,45,22,11,99])
    print (fibonacci' 9)
    print (reverse' [2,3,4,23,45,22,11,99])
    print (zip' [1,2,3] [1, 4, 9, 16])
    print (elem' 45 [2,3,4,23,45,22,11,99])
    print (elem' 0 [2,3,4,23,45,22,11,99])
    print (quicksort' [2,3,4,23,45,22,11,99])
    print (divideByTen 20)
    print (replicateCurriedThree 12)
    print (applyTwice (/ 2) 64)
    print (zipWith' (*) [1,2,3] [1, 4, 9, 16])
    print (partApp3 5 2)
    print (partApp1 (partApp3 5) 3)
    print (flip' zip [1,2,3,4,5] "moses")
    print (map' (^2) [1,2,3,4,5])
    print (filter' (>3) [1,5,3,2,1,6,4,3,2,1])
    print (quicksort'' [2,3,4,23,45,22,11,99])
    print (largestDiv10000 3829)
    print (takeWhile' (/=' ') "moses alexander")
    print (sum (takeWhile' (<10000) (filter odd (map (^2) [1..]))))
    print (collatzChain 10)
    print (numChainsGT 100 15)
    print (sum' [2,3,4,23,45,22,11,99])
    print (elem'' 4 [2,3,4,23,45,22,11,99])
    print (map'' (^2) [2,3,4,23,45,22,11,99])
    print (maximum'' [2,3,4,23,45,22,11,99])
    print $ filter'' (div' 3) [2,3,4,23,45,22,11,99]
    print $ map ($ 2) [(*2), (+6), (/2), sqrt, log]
    print $ map (negate . sum . tail) [[1..5],[3..6],[1..7]]
    print $ ceiling . negate . tan . cos . max 50 $ 3
    print $ replicate 3 . product . map (*3) . zipWith max [1,2,3,4,5] $ map (/2) [8, 10, 12, 14, 16]
    print oddSqSum
    print $ intersperseZero [1..10]
    print $ numUniques . replicate 3 $ 3
    print $ removeSpecialChars "møosßeßs"
    print $ surfaceA $ Rectangle 0 0 100 100
    print $ map (Circle 10 20) [1, 2, 3, 4]
    print $ surfaceA' $ Rectangle' (Point 0 0) (Point 100 100)
    print $ surfaceA' . shift (Circle' (Point 0 0) 1) 9 $ 9
    let person = Person' "moses" "a" 19 69 "345-6789" 'c'
    print person
    print $ bloodtype person
    print $ Vector' 1 2 3 `vplus` Vector' 3 2 1
    print $ pred Saturday
    print [minBound :: Day .. maxBound :: Day]
    print (pred $ succ minBound :: Day')
    dir <- getCurrentDirectory
    print dir
    (tmpName, tmpHandle) <- openTempFile "." "temp"
    withFile "/Users/moses/Documents/test1.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 1024)
        txt <- hGetContents handle
        putStrLn . head . lines $ txt)
    printAndParseTemps . getDirectoryContents $ "."
    removeFile tmpName
    printAndParseTemps . getDirectoryContents $ "."
    copyFile' "test.cabal" "testcabal.txt"
    removeFile "testcabal.txt"

    {- stack method testing
    let es = EmptyStack
    let s = stackPush 0 es
    let ss = stackPush 1 s
    let sss = stackPush 2 ss
    print sss
    print $ stackPeek (es :: Stack Int)
    print $ stackPeek s
    print $ stackPeek ss
    print $ stackPeek sss
    let (v1, ssss) = stackPop sss
    print v1
    print ssss
    let (v2, sssss) = stackPop ssss
    print v2
    print sssss
    let (v3, ssssss) = stackPop sssss
    print v3
    print ssssss
    let (v4, sssssss) = stackPop ssssss
    print v4
    print sssssss
    let (v5, ssssssss) = stackPop sssssss
    print v5
    print ssssssss
    print $ stackPeek ssssssss
    print sss
    end -}
    
    print $ fmap (++ " ... ") [" abc", " 123", " ..."]
    let p1 = fmap (*) (Just 3)
    print $ fmap (\f -> f 12) p1
    -- this lambda partially applies the arg 12 to the function wrapped in p1
    -- works bc fmap ... it maps the lambda to the value in the Maybe functor
    -- then applies the arg 12 and returns the Maybe evaluated on 12

    print $ applicativeTest' [1, 2, 3] (^) [1, 2, 3]
    print $ and' [True, True, True]
    print $ and' [True, False, True]
    print $ sequenceA [[1,2], [0]]
    print $ [(+1),(*100),(*5)] <*> [1,2,3]
    print $ concat ["abc", "def", "ghi"]
    print $ fmap (2*) (Just 2 :: Maybe Int)
    print $ Just (2*) <*> (Just 2 :: Maybe Int)
    print $ (*) <$> Just 2 <*> Just 2
    print $ Just 8 >>= (\x -> Just (x ^ 2))
    print $ canReachIn3 (6, 2) (6, 1)
    print $ multWithLog 4 16
    print $ gcd' 8 3
    print $ (*3) 3
    print $ parabola' 3
    print $ filterM keepSmall [1..10]
    print $ getPset [1, 2, 3]
    print $ getAllCoinVals [(True,0.2),(False,0.4),(True,0.3),(False,0.1)]
    print $ elemAtM [Left', Right', Left'] freeTree
    
    -- tree tests
    -- print $ (freeTree,[]) -: goLeft -: goRight -: modify' (const '~')
    -- print $ (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft -: treeAttach (Node 'Z' EmptyTree EmptyTree)
    -- print $ (freeTree,[]) -: goLeft -: goRight -: goTop
    -- end

    print "0"

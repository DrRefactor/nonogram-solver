module Nonogram where

import Control.Monad
import Data.List
import Data.Maybe
import Data.List.Split
import System.IO.Unsafe

type Row s = [s]
type Grid s = [Row s]

-- partial information about a square
type Square = Maybe Bool

-- readNonogram fileName = do
--         content <- readFile fileName
--         let blocks = lines content
--         return blocks

-- safePerformIO x = unsafePerformIO x

-- readNonogram :: String -> [[[Int]]]
readNonogram fileName = do
        content <- readFile fileName
        let contentLines = lines content
        let rowscols = splitWhen (\x -> x=="r" || x=="c") contentLines
        let fRowsCols = filter (/=[]) rowscols
        let mRowsCols = map (map splitToInt) fRowsCols
        return mRowsCols
        where   splitToInt arr = map toInt (words arr)
                toInt x        = read x :: Int

-- nonogramFromFile :: String -> IO
nonogramFromFile fileName = case unsafePerformIO (readNonogram fileName) of
        (rs:cs:_)   -> putStr (nonogram rs cs)
        _           -> putStr ("Invalid file")

-- Print the first solution (if any) to the nonogram
nonogram :: [[Int]] -> [[Int]] -> String
nonogram rs cs = case solve rs cs of
        [] -> "Inconsistent\n"
        (grid:_) -> showGrid rs cs grid

-- All solutions to the nonogram
-- using list comprehensions
-- usage examples:
-- > [x^2 | x <- [1..5]]
-- [1,4,9,16,25]
-- > [(x,y) | x <- [1,2], y <- ”AB” ]
-- [(1,'A'), (1,'B'), (2,'A'), (2,'B')]
-- join arguments, iterate right one first
solve :: [[Int]] -> [[Int]] -> [Grid Bool]
solve rs cs = [grid' |
                        -- deduce as many squares as we can
                grid <- maybeToList (deduction rs cs),
                        -- guess the rest, governed by rs
                grid' <- zipWithM (rowsMatching nc) rs grid,
                        -- check each guess against cs
                map contract (transpose grid') == cs]
  where nc = length cs
        contract = map length . filter head . group

-- A nonogram with all the values we can deduce
-- nr - number of rows
-- nc - number of cols
-- replicate n value - returns array of 'n' times 'value'
-- init to jest pusta macierz
-- zipWithM laczy dwa arraye z pomoca zdefiniowanej funkcji, tyle ze dla monadow, tak w array
-- zipWithM (+) [1,2,Nothing] [1,2,5] = [2,4,Notthing albo 5]
-- improve n = zipWith (+ n)
-- superfast operator >>= example(flatMap):
-- [1,2,3,4] >>= \ x -> [1, 2]
-- [1,2,1,2,1,2,1,2]
deduction :: [[Int]] -> [[Int]] -> Maybe (Grid Square)
deduction rs cs = converge step init
  where nr = length rs
        nc = length cs
        init = replicate nr (replicate nc Nothing)
        step = (improve nc rs . transpose) <.> (improve nr cs . transpose)
        improve n = zipWithM (common n)
        (g <.> f) x = f x >>= g

-- repeatedly apply f until a fixed point is reached
-- wywolowyanie f dla zwracanej wartosci (poczatkowo s, pozniej s'), dopoki wartosc nie przestanie sie zmieniac
converge :: (Monad m, Eq a) => (a -> m a) -> a -> m a
converge f s = do
        s' <- f s
        if s' == s then return s else converge f s'

-- common n ks partial = commonality between all possible ways of
-- placing blocks of length(s chyba) ks in a row of length n that match partial.
-- n - dlugosc kolumny lub wiersza
-- ks - bloczki
-- ? zaznaczenie miejsc, w ktorych musza byc bloczki, np wiersz o dlugosci 10 a bloczek o dlugosci 8, i srodkowe komorki
common :: Int -> [Int] -> Row Square -> Maybe (Row Square)
common n ks partial = case rowsMatching n ks partial of
        [] -> Nothing
        rs -> Just (foldr1 (zipWith unify) (map (map Just) rs))
  where unify :: Square -> Square -> Square
        unify x y
          | x == y = x
          | otherwise = Nothing

-- rowsMatching n ks partial = all possible ways of placing blocks of
-- length ks in a row of length n that match partial.
rowsMatching :: Int -> [Int] -> Row Square -> [Row Bool]
rowsMatching n [] [] = [[]]
rowsMatching n ks [] = []
rowsMatching n ks (Nothing:partial) =
        rowsMatchingAux n ks True partial ++
        rowsMatchingAux n ks False partial
rowsMatching n ks (Just s:partial) = 
        rowsMatchingAux n ks s partial

rowsMatchingAux :: Int -> [Int] -> Bool -> Row Square -> [Row Bool]
rowsMatchingAux n ks False partial =
        [False : row | row <- rowsMatching (n-1) ks partial]
-- n >= k straznik, zeby nie wyjsc poza wiersz
-- ostatni bloczek ([k])
-- 
rowsMatchingAux n [k] True partial =
        [replicate k True ++ replicate (n-k) False |
                n >= k && all (/= Just False) front && all (/= Just True) back]
  where (front, back) = splitAt (k-1) partial
-- dodajemy 'k' razy True, potem False (krzyzyk) i reszte wiersza
-- n > k + 1, czyli nie wychodzimy poza wiersz (bloczek musi sie zmiescic w wierszu + 1 wolne miejsce, bo mamy jeszcze bloczki w 'ks')
-- sprawdzamy front, czyli miejsca na ktore chcemy wpisac True, czy nie sa oznaczone jako False
-- sprawdzamy czy blank, czyli miejsce na przerwe nie jest oznaczone jako True
rowsMatchingAux n (k:ks) True partial =
        [replicate k True ++ False : row |
                n > k+1 && all (/= Just False) front && blank /= Just True,
                row <- rowsMatching (n-k-1) ks partial']
  where (front, blank:partial') = splitAt (k-1) partial

showGrid :: [[Int]] -> [[Int]] -> Grid Bool -> String
showGrid rs cs ss = unlines (zipWith showRow rs ss ++ showCols cs)
  where showRow rs ss = concat [['|', cellChar s] | s <- ss] ++ "| " ++
                unwords (map show rs)
        showCols cs
          | all null cs = []
          | otherwise = concatMap showCol cs : showCols (map advance cs)
        showCol (k:_)
          | k < 10 = ' ':show k
          | otherwise = show k
        showCol [] = "  "
        cellChar True = 'X'
        cellChar False = '_'
        advance [] = []
        advance (x:xs) = xs

test = nonogram [[6],[3,1,3],[1,3,1,3],[3,14],[1,1,1], [1,1,2,2],[5,2,2],[5,1,1],[5,3,3,3],[8,3,3,3]] [[4],[4],[1,5],[3,4],[1,5],[1],[4,1],[2,2,2], [3,3],[1,1,2],[2,1,1],[1,1,2],[4,1],[1,1,2], [1,1,1],[2,1,2],[1,1,1],[3,4],[2,2,1],[4,1]]
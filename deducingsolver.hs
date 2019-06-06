module Nonogram where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Tuple
import Data.List.Split
import System.IO.Unsafe
import System.Console.ANSI
import Data.Char

type Row s = [s]
type Grid s = [Row s]

-- partial information about a square
type Square = Maybe Bool
-- < 0 - no color
-- = 0 - dont know
-- > 0 - id of the color
type CSquare = Int
type CBlock = (Int, Int)

-- show color solution
palette :: [Color]
palette = [Black, Red, Green, Yellow, Blue, Magenta, Cyan, White]

readNonogram :: String -> IO [[[CBlock]]]
readNonogram fileName = do
        content <- readFile fileName
        let contentLines = lines content
        let rowscols = splitWhen (\x -> x=="r" || x=="c") contentLines
        let fRowsCols = filter (/=[]) rowscols
        let mRowsCols = map (map splitToTuple) fRowsCols
        return mRowsCols
        where   splitToTuple arr  = map toTuple (map (splitOn "-") (words arr))
                toTuple (x:y:_)   = (toInt x, toInt y)
                -- toTuple _         = (0,0)
                toInt x           = read x :: Int

nonogramFromFile :: String -> IO ()
nonogramFromFile fileName = case unsafePerformIO (readNonogram fileName) of
        (rs:cs:_)   -> putStr (nonogram rs cs)
        _           -> putStr ("Invalid file")

-- Print the first solution (if any) to the nonogram
nonogram :: [[CBlock]] -> [[CBlock]] -> String
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
-- group : [1,2,2,1,1] -> [[1],[2,2],[1,1]]
solve :: [[CBlock]] -> [[CBlock]] -> [Grid Int]
solve rs cs = [grid' |
                        -- deduce as many squares as we can
                grid <- maybeToList (deduction rs cs),
                        -- guess the rest, governed by rs
                grid' <- zipWithM (rowsMatching nc) rs grid,
                        -- check each guess against cs
                        -- grid' is [Row Int]
                        -- cs is columns - [[(int count,int color)]]
                (map contract (transpose grid')) == cs]
  where nc = length cs
        contract = map (\x -> (length x, head x)) . filter (\x -> (head x) > 0) . group
        -- contract = map length . filter (\x -> (head x) > 0) . group

-- A nonogram with all the values we can deduce
-- nr - number of rows
-- nc - number of cols
-- replicate n value - returns array of 'n' times 'value'
-- init to jest pusta macierz
-- zipWithM laczy dwa arraye z pomoca zdefiniowanej funkcji, tyle ze dla monadow, tak w array
-- zipWithM (+) [1,2,Nothing] [1,2,5] = [2,4,Notthing albo 5]
-- improve n = zipWith (+ n)
-- operator >>= example(flatMap):
-- [1,2,3,4] >>= \ x -> [1, 2]
-- [1,2,1,2,1,2,1,2]
deduction :: [[CBlock]] -> [[CBlock]] -> Maybe (Grid CSquare)
deduction rs cs = converge step init
  where nr = length rs
        nc = length cs
        init = replicate nr (replicate nc 0)
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
-- placing blocks of length(s) ks in a row of length n that match partial.
-- n - dlugosc kolumny lub wiersza
-- ks - bloczki
-- zaznaczenie miejsc, w ktorych musza byc bloczki,
--- np poczatkowo wiersz o dlugosci 10 a bloczek o dlugosci 8, i srodkowe komorki
common :: Int -> [CBlock] -> Row CSquare -> Maybe (Row CSquare)
common n ks partial = case rowsMatching n ks partial of
        [] -> Nothing
        rs -> Just (foldr1 (zipWith unify) (rs))
  where unify :: CSquare -> CSquare -> CSquare
        unify x y
          | x == y = x
          | otherwise = 0

-- rowsMatching n ks partial = all possible ways of placing blocks of
-- length ks in a row of length n that match partial.
rowsMatching :: Int -> [CBlock] -> Row CSquare -> [Row Int]
rowsMatching n [] [] = [[]]
rowsMatching n ks [] = []
rowsMatching n ks (0:partial) =
        rowsMatchingAux n ks 0 partial ++
        rowsMatchingAux n ks (-1) partial
rowsMatching n ks (s:partial) = 
        rowsMatchingAux n ks s partial

rowsMatchingAux :: Int -> [CBlock] -> Int -> Row CSquare -> [Row Int]
rowsMatchingAux n ks (-1) partial =
        [-1 : row | row <- rowsMatching (n-1) ks partial]
-- n >= k straznik, zeby nie wyjsc poza wiersz
-- ostatni bloczek ([k])
--  bylo 'c' w warunkach a jest 's'
-- all (/= s)
rowsMatchingAux n [(f, s)] c partial =
        [replicate f s ++ replicate (n-f) (0) |
                n >= f && all (\x -> x == 0 || x == s) front && all (\x -> x == 0 || x == (-1)) back]
  where (front, back) = splitAt (f-1) partial
-- dodajemy 'k' razy s, potem reszte wiersza
-- n > k + 1, czyli nie wychodzimy poza wiersz (bloczek musi sie zmiescic w wierszu + 1 wolne miejsce, bo mamy jeszcze bloczki w 'ks')
-- sprawdzamy front, czyli miejsca na ktore chcemy wpisac s, czy nie sa oznaczone jako False
-- sprawdzamy czy blank, czyli miejsce na przerwe nie jest oznaczone jako s
--- bylo 'c' w warunkach a jest 's'
rowsMatchingAux n ((f, s):ks) c partial =
        -- [replicate f s ++ (-1) : row |
        [(replicate f s) ++ row |
                n > f+1 && all (\x -> x == 0 || x == s) front && blank /= s,
                row <- rowsMatching (n-f) ks (blank:partial')]
  where (front, blank:partial') = splitAt (f-1) partial

showGrid :: [[CBlock]] -> [[CBlock]] -> Grid Int -> String
showGrid rs cs ss = unlines (zipWith showRow rs ss ++ showCols cs)
  where showRow rs ss = concat [['|', cellChar s] | s <- ss] ++ "| " ++
                unwords (map show rs)
        showCols :: [[CBlock]] -> [[Char]]
        showCols cs
          | all null cs = []
          | otherwise = concatMap showCol cs : showCols (map advance cs)
        showCol ((k, l):_)
          | k < 10 = ' ':show k
          | otherwise = show k
        showCol [] = "  "
        cellChar (-1) = '_'
        cellChar 0 = '_'
        cellChar c = head (show c)
        advance [] = []
        advance (x:xs) = xs

putCharWithColor :: Char -> Color -> IO () 
putCharWithColor x c = do
        setSGR [SetColor Foreground Vivid c]
        putChar x
        setSGR [Reset]

test :: [Color] -> IO ()
test [] = putChar '\n'
test (c:cs) = do putCharWithColor 'X' c
                 test cs 
        
test1 = nonogram [[(6, 1)],[(3, 1),(1, 1),(3, 1)],[(1, 1),(3, 1),(1, 1),(3, 1)],[(3, 1),(14, 1)],[(1, 1),(1, 1),(1, 1)], [(1, 1),(1, 1),(2, 1),(2, 1)],[(5, 1),(2, 1),(2, 1)],[(5, 1),(1, 1),(1, 1)],[(5, 1),(3, 1),(3, 1),(3, 1)],[(8, 1),(3, 1),(3, 1),(3, 1)]] [[(4, 1)],[(4, 1)],[(1, 1),(5, 1)],[(3, 1),(4, 1)],[(1, 1),(5, 1)],[(1, 1)],[(4, 1),(1, 1)],[(2, 1),(2, 1),(2, 1)], [(3, 1),(3, 1)],[(1, 1),(1, 1),(2, 1)],[(2, 1),(1, 1),(1, 1)],[(1, 1),(1, 1),(2, 1)],[(4, 1),(1, 1)],[(1, 1),(1, 1),(2, 1)], [(1, 1),(1, 1),(1, 1)],[(2, 1),(1, 1),(2, 1)],[(1, 1),(1, 1),(1, 1)],[(3, 1),(4, 1)],[(2, 1),(2, 1),(1, 1)],[(4, 1),(1, 1)]]
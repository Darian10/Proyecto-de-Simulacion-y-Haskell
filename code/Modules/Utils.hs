module Modules.Utils
(
    dir_x,
    dir_y,
    is_valid_pos,
    rand_list,
    random_dir,
    empty_celds_percent,
    random_adj_celds,
    concat_lists,
    is_visited,
    random',
    write_in_txt
)
where
    
import Modules.Elements

import System.Random
import Data.List
import System.IO.Unsafe


dir_x :: [Int]
dir_x = [1, 0, -1,  0]

dir_y :: [Int]
dir_y = [0, 1,  0, -1]


is_valid_pos :: Int -> Int -> Int -> Int -> Bool
is_valid_pos x y n m = if x >= 0 && x < n && y >= 0 && y < m then True else False


rand_list :: Int -> (Int, Int) -> StdGen -> [Int]
rand_list n (min, max) rand_gen = if n > (max - min + 1) then error "full env" else take n $ nub $ randomRs (min, max) rand_gen :: [Int]


random_dir :: StdGen -> (Int,Int,StdGen)
random_dir rand_gen = let (i,new_rand_gen) = randomR (0,3) rand_gen in (dir_x !! i, dir_y !! i, new_rand_gen)


--- devuelve un random entre dos numeros
random' :: (Int,Int) -> Int
random' (min,max) = unsafePerformIO (getStdRandom (randomR (min, max)))


--- devuelve una permutacion random de una lista
random_perm :: [a] -> [a]
random_perm list = let
                        perm_list = permutaciones list
                        rand = random' (0, length(perm_list)-1)
                    in perm_list !! rand


----- Para las permutaciones
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = 
    concat [intercala x ys | ys <- permutaciones xs]

----------------------------------------------------------



empty_celds_percent :: [Element] -> [(Int,Int)] -> Int
empty_celds_percent dirty_celds empty_celds =  
                                let 
                                    empty = length empty_celds
                                    dirty = length dirty_celds
                                in div (empty * 100) (dirty+empty)

index_of :: (Eq a) => a -> [a] -> Int
index_of elem list = 
        let
            index_of' _ [] _ = -1
            index_of' elem (x:xs) i = if elem == x then i else index_of' elem xs (i+1)
        in index_of' elem list 0


random_adj_celds :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int -> Int -> Int -> StdGen -> [(Int,Int)]
random_adj_celds [] _ _ _ _ _ _ = []
random_adj_celds stack@((x,y):xs) visited c child n m rand_gen
    | c == child = visited
    | otherwise = random_adj_celds (adj ++ xs) new_visited (c+1) child n m new_rand_gen
        where 
            new_visited = visited ++ [(x,y)]
            adj = [((a i),(b i)) | i <- (rand_list 4 (0,3) rand_gen), is_valid_pos (a i) (b i) (n-2) m, not (elem ((a i),(b i)) visited)]
            (_,new_rand_gen) = randomR (0,3) rand_gen :: (Int,StdGen)

            a i = x + dir_x!!i
            b i = y + dir_y!!i
  
   


concat_lists :: [Element] -> [Element] -> [Element] -> [Element] -> [Element] -> [Element] -> [Element]
concat_lists a b c d e f = sort (a ++ b ++ c ++ d ++ e ++ f)


is_visited :: Int -> Int -> [(Element,Element)] -> Bool
is_visited x y [] = False
is_visited x y ((_ , (Element _ (Position lx ly))) : xs)
        | x == lx && y == ly = True
        | otherwise = is_visited x y xs


write_in_txt :: String -> IO()
write_in_txt line = appendFile "output.txt" (line ++ "\n")
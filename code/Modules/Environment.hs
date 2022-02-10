module Modules.Environment
(
    add_type_elements,
    create_empty_celds,
    update_celds,
    change_env,
    show_env,
    get_empty_celds_as_element,
    create_playpen,
    move_child
) 
where

import Modules.Elements
import Modules.Utils

import System.Random
import Data.List


                            
                                         
--devuelve una lista de n elementos en posiciones aleatorias del ambiente
add_type_elements :: String -> Int -> [(Int,Int)] -> StdGen -> [Element]
add_type_elements name n emptyPos rand_gen = let indexes = rand_list n (0, ((length emptyPos) - 1)) rand_gen
                                    in  [create_element name (emptyPos !! i) | i <- indexes]



create_empty_celds :: Int -> Int -> [(Int,Int)]
create_empty_celds n m = [(a,b) | a <- [0..n-1], b <- [0..m-1]]


-- elimina de la segunda lista los elementos de la primera
update_celds :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
update_celds [] list = list 
update_celds ((a,b):xs) list = update_celds xs (delete_celd (a,b) list)


delete_celd :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
delete_celd (x,y) [] = []
delete_celd (x,y) ((a,b): xs) = if a == x && b == y then delete_celd (x,y) xs else (a,b): delete_celd (x,y) xs



child_can_move :: Element -> Int -> Int -> Int -> Int -> (Int,Int) -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> Bool
child_can_move child@(Element _ (Position cx cy)) x y n m (dx,dy) empty_celds obst_celds robot_celds playpen
            | elem (cx,cy) (get_celds_elements playpen) = False
            | elem (cx,cy) (get_celds_elements robot_celds) && name (get_elem (cx,cy) robot_celds) == "Robot charging" = False 
            | not $ is_valid_pos x y n m = False
            | elem (x,y) empty_celds = True
            | elem (x,y) (get_celds_elements obst_celds) = child_can_move child (x+dx) (y+dy) n m (dx,dy) empty_celds obst_celds robot_celds playpen
            | otherwise = False


update_empty_celds :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
update_empty_celds x y newx newy dx dy empty_celds = if elem (newx,newy) empty_celds 
                                  then (x,y):(update_celds [(newx,newy)] empty_celds)
                                  else update_empty_celds x y (newx + dx) (newy + dy) dx dy empty_celds

-- cuadricula de 3x3
grid = [(0,0),(0,-1),(0,-2),(-1,0),(-1,-1),(-1,-2),(-2,0),(-2,-1),(-2,-2)]

random_grid :: Int -> Int -> Int -> Int -> StdGen -> (Int,Int)
random_grid x y n m rand_gen = let 
                                valid_grids = filter is_valid_grid [((dx+x),(dy+y)) | (dx,dy) <- grid]
                                (i,_) = randomR (0,((length valid_grids)-1)) rand_gen
                            in  valid_grids !! i
                            where
                                is_valid_grid :: (Int,Int) -> Bool
                                is_valid_grid (x,y)
                                            | is_valid_pos x y n m && is_valid_pos (x+2) (y+2) n m = True
                                            | otherwise = False


filter_celd :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
filter_celd gx gy [] = []
filter_celd gx gy ((x,y):xs) = 
                let
                    inside_grid :: Int -> Int -> Int -> Int -> Bool
                    inside_grid gx gy x y = if x <= (gx+2) && x >= gx && y <= (gy+2) && y >= gy then True else False
                in if inside_grid gx gy x y then (x,y):filter_celd gx gy xs else filter_celd gx gy xs

celd_in_grid ::  Int -> Int -> [Element] -> Int
celd_in_grid gx gy list =  length in_grid
                        where
                            pos = get_celds_elements list
                            in_grid = filter_celd gx gy pos 

 

rand_dirty_celds_in_grid :: Int -> Int -> [(Int,Int)] -> [Element] -> StdGen -> [(Int,Int)]
rand_dirty_celds_in_grid gx gy empty_celds child_celds rand_gen = let 
                            children_in_grid = celd_in_grid gx gy child_celds
                            empty_celds_in_grid = filter_celd gx gy empty_celds

                            (dirty_number,_) = get_rand_dirty_celds_in_grid_number children_in_grid (length empty_celds_in_grid) rand_gen
                            dirty_celd = [empty_celds_in_grid !! i | i <- (indexes dirty_number empty_celds_in_grid)]
        
                            in dirty_celd

                where   indexes dirty_number empty_celds_in_grid = rand_list dirty_number (0, ((length empty_celds_in_grid)-1)) rand_gen
                        get_rand_dirty_celds_in_grid_number children empty_celds rand_gen 
                            | children == 1 = randomR (0,(min 1 empty_celds)) rand_gen
                            | children == 2 = randomR (0,(min 3 empty_celds)) rand_gen 
                            | otherwise = randomR (0,(min 6 empty_celds)) rand_gen



change_env :: [Element] -> [Element] -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> Int -> Int -> Int -> StdGen -> ([Element],[Element],[Element],[(Int,Int)])
change_env child_celds dirty_celds obst_celds empty_celds robot_celds playpen n m i rand_gen
        | i == length child_celds = (child_celds, dirty_celds, obst_celds, empty_celds)
        |otherwise = change_env new_child_celds new_dirty_celds new_obst_celds new_new_empty_celds robot_celds playpen n m (i+1) new_rand_gen
            where 
                child = child_celds !! i
                (dx, dy, new_rand_gen) = random_dir rand_gen
                x = row $ position child
                y = col $ position child

                can_move = child_can_move child (x+dx) (y+dy) n m (dx,dy) empty_celds obst_celds robot_celds playpen

                new_child_celds = if can_move then move_child child dx dy child_celds else child_celds

                new_obst_celds = if can_move
                              then move_obstacles (x+dx) (y+dy) (x+dx) (y+dy) obst_celds empty_celds dx dy
                              else obst_celds

                new_empty_celds = if can_move
                             then if elem (x,y) (get_celds_elements robot_celds)
                                  then delete (x,y) (update_empty_celds x y (x+dx) (y+dy) dx dy empty_celds)
                                  else update_empty_celds x y (x+dx) (y+dy) dx dy empty_celds
                             else empty_celds


                (new_dirty_celds,new_new_empty_celds) = 
                    if can_move
                    then let (gx,gy) = random_grid x y n m rand_gen
                             celd_to_dirt = rand_dirty_celds_in_grid gx gy new_empty_celds child_celds rand_gen
                             dirty_elements = [(create_element "Dirty" position) | position <- celd_to_dirt]

                         in ((dirty_celds ++ dirty_elements), (update_celds celd_to_dirt new_empty_celds))
                             
                    else (dirty_celds,new_empty_celds) 



create_playpen child n m g1 g2 = 
                    let 
                        (x,_) = randomR (0, ((n-1)-2)) g1
                        (y,_) = randomR (0,  (m-1))    g2

                    in random_adj_celds [(x,y)] [] 0 child n m g1
                        


move_child :: Element -> Int -> Int -> [Element] -> [Element]
move_child child dx dy [] = []
move_child child@(Element _ (Position x y)) dx dy (current_child@(Element _ (Position cur_x cur_y)):xs) =
                                if cur_x == x && cur_y == y
                                then (move_element child dx dy): move_child child dx dy xs
                                else current_child: move_child child dx dy xs


move_obstacles :: Int -> Int -> Int -> Int -> [Element] -> [(Int,Int)] -> Int -> Int -> [Element]
move_obstacles x y cur_x cur_y obst_celds empty_celds dx dy
            | elem (x,y) empty_celds = update_elem (x,y) (cur_x,cur_y) obst_celds "Obstacle"
            | otherwise = move_obstacles (x+dx) (y+dy) cur_x cur_y obst_celds empty_celds dx dy



insert_in_env :: String -> Int -> Int -> Int -> [[String]] -> [[String]]
insert_in_env _ _ _ _ [] = []
insert_in_env name x y i env@(l:ls)
    | x == i = (get_new_list name y 0 l): insert_in_env name x y (i+1) ls
    | otherwise = l: insert_in_env name x y (i+1) ls
    where
        get_new_list _ _ _ [] = []
        get_new_list name y j list@(x:xs)
                | j == y = (add_spaces $ add (name !! 0) (remove_spaces x)) : get_new_list name y (j+1) xs
                | otherwise = (add_spaces (remove_spaces x)) : get_new_list name y (j+1) xs
                where 
                    remove_spaces s = filter (\x -> x /= ' ') s 
                    add_spaces s 
                        | length s >= 3 = s
                        | length s == 2 = s ++ " "
                        | length s == 1 = " " ++ s ++ " "
                        | otherwise = "   "
                    add c s = s ++ [c]


print_env :: [[String]] -> IO()
print_env [] = return ()
print_env (l:ls) = do
                        putStrLn (concat (intersperse " " l))
                        putStrLn (concat $ take (length l) (repeat "    "))
                        print_env ls

get_empty_env :: Int -> Int -> [[String]]
get_empty_env 0 m = []
get_empty_env n m = (take m $ repeat ""): get_empty_env (n-1) m


get_env :: [Element] -> [[String]] -> [[String]]
get_env [] env = env
get_env (elem@(Element name (Position x y)):ls) env = get_env ls (insert_in_env name x y 0 env)


get_empty_celds_as_element :: [(Int,Int)] -> [Element]
get_empty_celds_as_element [] = []
get_empty_celds_as_element ((x,y):xs) = (Element " " (Position x y)) :  get_empty_celds_as_element xs


-- metodo principal para pintar el tablero
show_env :: [Element] -> [Element] -> [Element] -> [Element] -> [Element] -> [(Int,Int)] -> Int -> Int -> IO ()
show_env playpen robot_celds child_celds dirty_celds obst_celds empty_celds n m = do
    let empty_celds_elem = get_empty_celds_as_element empty_celds
    let empty = get_empty_env n m
    let order_list = concat_lists playpen robot_celds child_celds dirty_celds obst_celds empty_celds_elem
    let env = get_env order_list empty

    putStrLn("\n")
    print_env (env)
    putStrLn("\n")

module Modules.Robots ( robot_smart ) where

import Modules.Elements
import Modules.Environment
import Modules.Utils

import Data.List
import System.Random
        


robot_smart :: Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> Int -> Int -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
robot_smart robot@(Element name (Position x y)) child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
        | (empty_celds_percent dirty_celds empty_celds) <= 75 = 
                clean_env robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
        
        | otherwise = 
                put_childs_in_playpen robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m



put_childs_in_playpen :: Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> Int -> Int -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
put_childs_in_playpen robot@(Element name (Position x y)) child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
        | state == "free" = 
                let
                (child, child_visited) = less_distance_to_elems  [robot] [] n m empty_celds dirty_celds playpen child_celds robot_celds "Child"
                (dirty, dirty_visited) = less_distance_to_elems  [robot] [] n m empty_celds dirty_celds playpen child_celds robot_celds "Dirty"

                child_path = get_path_to_move_robot child child_visited
                dirty_path = get_path_to_move_robot dirty dirty_visited

                reachable_child = if child_path == [] then False else True
                reachable_dirty = if dirty_path == [] then False else True

                in      if reachable_child 
                        then move_robot_searching_child  robot child_path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                        else    if reachable_dirty
                                then move_robot_dirty robot dirty_path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                                else robot_random robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m

        | otherwise = 
                let
                
                (play, play_visited) = less_distance_to_elems  [robot] [] n m empty_celds dirty_celds playpen child_celds robot_celds "Playpen"
                play_path = get_path_to_move_robot play play_visited

                reachable_play = if play_path == [] then False else True

                in      if robot_in_playpen
                        then    if playpen_in_pos_up
                                then robot_finding_better_pos_to_put_child   robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                                else robot_put_child robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                        
                        else    if reachable_play
                                then move_robot_with_child  robot play_path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                                else robot_random robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m

                

     where      state = get_robot_state robot
                robot_in_playpen = elem (x,y) (get_celds_elements playpen)
                playpen_in_pos_up = elem ((x-1),y) (get_celds_elements playpen) &&  robot_can_move (x-1) y empty_celds dirty_celds playpen child_celds robot_celds "" 



clean_env :: Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> Int -> Int -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
clean_env robot@(Element name (Position x y)) child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
        | state == "free" = 
                if reachable_dirty 
                then move_robot_dirty robot dirty_path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                else    if reachable_child
                        then    if elem (x,y) (get_celds_elements dirty_celds)
                                then    let new_dirty_celds = delete (get_elem (x,y) dirty_celds) dirty_celds
                                        in (child_celds, new_dirty_celds, robot_celds, empty_celds, rand_gen)
                                else  move_robot_searching_child  robot child_path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
        
                        else robot_random robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
        
        | otherwise = 
                if reachable_dirty
                then robot_put_child robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                else    if robot_in_playpen
                        then    if playpen_in_pos_up
                                then robot_finding_better_pos_to_put_child   robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                                else robot_put_child robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen

                        else    if reachable_play
                                then move_robot_with_child  robot play_path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
                                else robot_random robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m

        where
                state = get_robot_state robot
                robot_in_playpen = elem (x,y) (get_celds_elements playpen)
                playpen_in_pos_up = elem ((x-1),y) (get_celds_elements playpen) && robot_can_move (x-1) y empty_celds dirty_celds playpen child_celds robot_celds ""

                (child, child_visited) = less_distance_to_elems  [robot] [] n m empty_celds dirty_celds playpen child_celds robot_celds "Child"
                (dirty, dirty_visited) = less_distance_to_elems  [robot] [] n m empty_celds dirty_celds playpen child_celds robot_celds "Dirty"

                child_path = get_path_to_move_robot child child_visited
                dirty_path = get_path_to_move_robot dirty dirty_visited

                reachable_child = if child_path == [] then False else True
                reachable_dirty = if dirty_path == [] then False else True

                (play, play_visited) = less_distance_to_elems  [robot] [] n m empty_celds dirty_celds playpen child_celds robot_celds "Playpen"
                play_path = get_path_to_move_robot play play_visited

                reachable_play = if play_path == [] then False else True




robot_random :: Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> Int -> Int -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
robot_random robot@(Element name (Position x y)) child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
        | elem (x,y) (get_celds_elements dirty_celds) = 
                let new_dirty_celds = delete (Element "Dirty" (Position x y)) dirty_celds
                in (child_celds, new_dirty_celds, robot_celds, empty_celds, rand_gen)

        | otherwise =
                let 
                        adj = [((dir_x!!i),(dir_y!!i)) | i <- [0..3], is_valid_pos (a i) (b i) n m, 
                                                        robot_can_move (a i) (b i) empty_celds dirty_celds playpen child_celds robot_celds ""]
                        a i = x + dir_x!!i
                        b i = y + dir_y!!i

                in      if adj == []
                        then (child_celds, dirty_celds, robot_celds, empty_celds, rand_gen)
                        else let
                                (i,new_rand_gen) = randomR (0, ((length adj)-1)) rand_gen
                                (dx,dy) = adj !! i

                                newx = x + dx
                                newy = y + dy

                                new_child_celds =  if (get_robot_state robot) == "charging"
                                                then let child = get_elem (x,y) child_celds
                                                     in move_child child dx dy child_celds
                                                else child_celds

                                (new_robot_celds, new_empty_celds) = move_robot robot newx newy new_child_celds empty_celds playpen robot_celds dirty_celds
                                
                        in (new_child_celds, dirty_celds, new_robot_celds, new_empty_celds, new_rand_gen) 
   



robot_can_move :: Int -> Int -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> [Element] -> String -> Bool
robot_can_move x y empty_celds dirty_celds playpen child_celds robot_celds name_Element
  | elem (x,y) (get_celds_elements robot_celds) = False
  | elem (x,y) empty_celds || elem (x,y) (get_celds_elements dirty_celds) = True
  | elem (x,y) (get_celds_elements playpen) && not ( elem (x,y) (get_celds_elements child_celds) ) = True
  | elem (x,y) (get_celds_elements child_celds) && name_Element == "Child" && not ( elem (x,y) (get_celds_elements playpen) ) = True
  | otherwise = False




less_distance_to_elems  :: [Element] -> [(Element,Element)] -> Int -> Int -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> [Element] -> String -> (Element,[(Element,Element)])
less_distance_to_elems  [] visited _ _ _ _ _ _ _ _= (Element "null" (Position (-1) (-1)),[])
less_distance_to_elems  queue@(elem@(Element name (Position x y)):xs) visited n m empty_celds dirty_celds playpen child_celds robot_celds name_Element
    | name == name_Element = (elem, visited)
    | otherwise = less_distance_to_elems  new_queue new_visited n m empty_celds dirty_celds playpen child_celds robot_celds name_Element   
            where 
                adj = [((a i),(b i)) | i <- [0..3], is_valid_pos (a i) (b i) n m, 
                                                robot_can_move (a i) (b i) empty_celds dirty_celds playpen child_celds robot_celds name_Element, 
                                                not (is_visited (a i) (b i) visited)]

                elems_list = elem_from_pos  ((get_empty_celds_as_element empty_celds) ++ dirty_celds ++ playpen ++ child_celds) adj
                new_queue = xs ++ elems_list
                new_visited = visited ++ zip (repeat elem) elems_list

                a i = x + dir_x!!i
                b i = y + dir_y!!i 

                   


-- devuelve la mejor posicion para llegar al elemento que se pasa como parametro
get_path_to_move_robot :: Element -> [(Element,Element)] -> [Element]
get_path_to_move_robot elem visited
        | name elem == "null" = []
        | head(name parent) == 'R' = [elem]
        | otherwise = get_path_to_move_robot parent visited ++ [elem]

        where 
            parent = parent_of elem visited

            parent_of :: Element -> [(Element,Element)] -> Element
            parent_of _ [] = Element "null" (Position (-1) (-1))
            parent_of elem ((parent,child):xs)
                | elem == child = parent
                | otherwise = parent_of elem xs


get_robot_state :: Element -> String
get_robot_state (Element name _) = (words name) !! 1


move_robot :: Element -> Int -> Int -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> ([Element],[(Int,Int)])
move_robot (Element name (Position x y)) newx newy child_celds empty_celds playpen robot_celds dirty_celds =
        let
                new_robot_celds = update_elem (newx,newy) (x,y) robot_celds name
                new_empty_celds = delete (newx,newy) empty_celds
                new_empty_celds' =   if elem (x,y) (get_celds_elements (child_celds ++ playpen ++ dirty_celds))
                                then new_empty_celds
                                else (x,y):new_empty_celds
        in      
                (new_robot_celds, new_empty_celds')


move_robot_searching_child  :: Element -> [Element] -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> ([Element],[Element],[Element],[(Int,Int)], StdGen)
move_robot_searching_child  robot@(Element name (Position x y)) path child_celds empty_celds dirty_celds playpen robot_celds rand_gen =
        let 
                newx = row $ position (path !! 0)
                newy = col $ position (path !! 0)
                new_robot =      if elem (newx,newy) (get_celds_elements child_celds)
                                then Element "Robot charging" (Position x y)
                                else robot
                (new_robot_celds, new_empty_celds) = move_robot new_robot newx newy child_celds empty_celds playpen robot_celds dirty_celds

        in (child_celds, dirty_celds, new_robot_celds, new_empty_celds, rand_gen)



robot_finding_better_pos_to_put_child   :: Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
robot_finding_better_pos_to_put_child   robot@(Element name (Position x y)) child_celds empty_celds dirty_celds playpen robot_celds rand_gen =
        let two_steps = elem ((x-2),y) (get_celds_elements playpen) && robot_can_move (x-2) y empty_celds dirty_celds playpen child_celds robot_celds ""
            new_child_celds =  let child = get_elem (x,y) child_celds
                            in  if two_steps
                                then move_child child (-2) 0 child_celds
                                else move_child child (-1) 0 child_celds
            (new_robot_celds, new_empty_celds) = 
                if two_steps
                then move_robot robot (x-2) y new_child_celds empty_celds playpen robot_celds dirty_celds
                else move_robot robot (x-1) y new_child_celds empty_celds playpen robot_celds dirty_celds

        in (new_child_celds, dirty_celds, new_robot_celds, new_empty_celds, rand_gen)


robot_put_child :: Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
robot_put_child robot@(Element name (Position x y)) child_celds empty_celds dirty_celds playpen robot_celds rand_gen =
        let new_robot = Element "Robot free" (Position x y)
            (new_robot_celds, new_empty_celds) = move_robot new_robot x y child_celds empty_celds playpen robot_celds dirty_celds

        in (child_celds, dirty_celds, new_robot_celds, new_empty_celds, rand_gen)



move_robot_dirty :: Element -> [Element] -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
move_robot_dirty robot@(Element name (Position x y)) path child_celds empty_celds dirty_celds playpen robot_celds rand_gen
        | elem (x,y) (get_celds_elements dirty_celds) = 
                let new_dirty_celds = delete (Element "Dirty" (Position x y)) dirty_celds
                in (child_celds, new_dirty_celds, robot_celds, empty_celds, rand_gen)
        
        | otherwise = let
                        newx = row $ position (path !! 0)
                        newy = col $ position (path !! 0)
                        (new_robot_celds, new_empty_celds) = move_robot robot newx newy child_celds empty_celds playpen robot_celds dirty_celds
               
                      in (child_celds, dirty_celds, new_robot_celds, new_empty_celds, rand_gen)



move_robot_with_child  :: Element -> [Element] -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> ([Element],[Element],[Element],[(Int,Int)],StdGen)
move_robot_with_child  robot@(Element name (Position x y)) path child_celds empty_celds dirty_celds playpen robot_celds rand_gen =
        let two_steps = (length path) >= 2

            newx = if two_steps then row $ position (path !! 1) else row $ position (path !! 0)
            newy = if two_steps then col $ position (path !! 1) else col $ position (path !! 0)

            new_child_celds = let child = get_elem (x,y) child_celds
                           in move_child child (newx-x) (newy-y) child_celds

            (new_robot_celds, new_empty_celds) = move_robot robot newx newy new_child_celds empty_celds playpen robot_celds dirty_celds

        in (new_child_celds, dirty_celds, new_robot_celds, new_empty_celds, rand_gen)
        



module Modules.Elements( 
    Element(..),
    Position(..),
    create_element,
    move_element,
    elem_from_pos ,
    update_elem,
    get_elem,
    get_celds_elements
) 
where

import System.Random
import Data.List




data Element = Element { name :: String, position :: Position} deriving (Show,Eq)

data Position = Position {row :: Int, col :: Int} deriving (Show,Eq)


instance Ord Element where
    (<) (Element _ (Position x1 y1)) (Element _ (Position x2 y2))
        |x1 < x2 = True
        |x1 == x2 = y1 < y2
        |otherwise = False

    (<=)(Element _ (Position x1 y1)) (Element _ (Position x2 y2))
        |x1 < x2 = True
        |x1 == x2 = y1 <= y2
        |otherwise = False

    (>) e1 e2 = not (e1 <= e2) 
    (>=) e1 e2 = not (e1 < e2)


create_element :: String -> (Int,Int) -> Element
create_element name (x,y) = Element name $ Position x y



move_element :: Element -> Int -> Int -> Element
move_element (Element name (Position x y)) d1 d2 = Element name $ Position (x + d1) (y + d2)


update_elem :: (Int,Int) -> (Int,Int) -> [Element] -> String -> [Element]
update_elem _ _ [] _ = []
update_elem (x,y) (cur_x,cur_y) (elem@(Element _ (Position lx ly)):xs) name = 
                                if lx == cur_x && ly == cur_y
                                then (Element name (Position x y)): update_elem (x,y) (cur_x,cur_y) xs name
                                else elem: update_elem (x,y) (cur_x,cur_y) xs name

get_elem :: (Int,Int) -> [Element] -> Element
get_elem _ [] = Element "null" (Position (-1) (-1))
get_elem (x,y) (elem@(Element _ (Position ex ey)):xs) = if ex == x && ey == y then elem else get_elem (x,y) xs


get_celds_elements :: [Element] -> [(Int,Int)]
get_celds_elements [] = []
get_celds_elements ((Element name (Position x y)):xs) = (x,y):get_celds_elements xs


elem_from_pos  :: [Element] -> [(Int,Int)] -> [Element]
elem_from_pos  _ [] = []
elem_from_pos  elem_list ((x,y):ys) = get_element elem_list (x,y) ++ elem_from_pos  elem_list ys
                            where
                                get_element :: [Element] -> (Int,Int) -> [Element]
                                get_element [] _ = [] 
                                get_element (elem@(Element _ (Position ex ey)):xs) (x,y) =  if x == ex && y == ey 
                                                                                        then [elem] 
                                                                                        else get_element xs (x,y)



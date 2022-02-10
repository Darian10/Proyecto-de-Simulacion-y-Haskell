import Data.Char
import System.Random
import Control.Exception


import Modules.Elements
import Modules.Utils
import Modules.Environment
import Modules.Robots

main = do
        putStrLn ("Type input values separated by space in the following way:")
        putStrLn ("<n> <m> <# obstacles> <# dirty> <# childrens> <# robots> <t_change_env> <# turns> <# simulations>")

        
        input <- getLine
        let input_list = words input
        let n = read (input_list !! 0) :: Int
        let m = read (input_list !! 1) :: Int
        let obst_number = read (input_list !! 2) :: Int
        let dirty_number = read (input_list !! 3) :: Int
        let childrens_number = read (input_list !! 4) :: Int
        let robots_number = read (input_list !! 5) :: Int
        let t = read (input_list !! 6) :: Int
        let turns_number = read (input_list !! 7) :: Int
        let simulations = read (input_list !! 8) :: Int


        let total_celds = n*m

        if total_celds >= (obst_number+dirty_number+2*childrens_number+robots_number)
        then do
            write_in_txt ("<n> <m> <# obstacles> <# dirty> <# childrens> <# robots> <t_change_env> <# turns> <# simulations>")
            write_in_txt input

            create_env n m robots_number childrens_number dirty_number obst_number t turns_number 1 simulations []
        else 
            putStrLn ("ERROR: More values than celds") 




simulation :: [Element] -> [Element] -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> Int -> Int -> Int -> Int -> StdGen -> Int -> Int ->
    (Element -> [Element] -> [(Int,Int)] -> [Element] -> [Element] -> [Element] -> StdGen -> Int -> Int -> ([Element],[Element],[Element],[(Int,Int)],StdGen)) -> Int -> Int -> Int -> [Int] -> IO()
simulation child_celds robot_celds dirty_celds empty_celds obst_celds playpen t turn turns_number robot_index rand_gen n m robot_smart dirty_number sim all_sim percent_clean 
    
    | turn == turns_number = 
        let 
            in_playpen = [child | child <- (get_celds_elements child_celds), elem child (get_celds_elements playpen)]
            in_robot = [child | child <- (get_celds_elements child_celds), elem child (get_celds_elements robot_celds)]
            in_both = [child | child <- (get_celds_elements child_celds), elem child (get_celds_elements playpen),
                                                                        elem child (get_celds_elements robot_celds)] 
        in (
            if ( (length in_playpen) + (length in_robot) - (length in_both) ) == (length child_celds) 
            then 
                do
                    putStrLn ("Sim: " ++ (show sim) )
                    write_in_txt ("\nSim: " ++ (show sim) )

                    putStrLn ("All childs in playpen or in robots")
                    write_in_txt "All childs in playpen or in robots"

                    let percent = empty_celds_percent dirty_celds empty_celds
                    if percent < 60
                    then 
                        do
                            putStrLn ("Environment dirty")
                            write_in_txt "Environment dirty"

                            putStrLn ("Clean percent: " ++ (show percent) ++ "%")
                            write_in_txt ("Clean percent: " ++ (show percent) ++ "%")
                    else 
                        do
                            putStrLn ("Environment clean")
                            write_in_txt "Environment clean"

                            putStrLn ("Clean percent: " ++ (show percent) ++ "%")
                            write_in_txt ("Clean percent: " ++ (show percent) ++ "%")

                    create_env n m (length robot_celds) (length child_celds) dirty_number (length obst_celds) t turns_number (sim+1) all_sim (percent_clean ++ [percent])
            else
                do 
                    putStrLn ("Sim: " ++ (show sim) )
                    write_in_txt ("\nSim: " ++ (show sim) )

                    putStrLn("Some child are free" )
                    write_in_txt "Some child are free"

                    let percent = empty_celds_percent dirty_celds empty_celds
                    if percent < 60
                    then 
                        do
                            putStrLn ("Environment dirty")
                            write_in_txt "Environment dirty"

                            putStrLn ("Clean percent: " ++ (show percent) ++ "%")
                            write_in_txt ("Clean percent: " ++ (show percent) ++ "%")

                    else 
                        do
                            putStrLn ("Environment clean")
                            write_in_txt "Environment clean"

                            putStrLn ("Clean percent: " ++ (show percent) ++ "%")
                            write_in_txt ("Clean percent: " ++ (show percent) ++ "%")

                    create_env n m (length robot_celds) (length child_celds) dirty_number (length obst_celds) t turns_number (sim+1) all_sim (percent_clean ++ [percent]) )
        

    | robot_index  == (length robot_celds) = 
            if (mod turn t) == 0
            then let  (new_child_celds, new_dirty_celds, new_obst_celds, new_empty_celds) =
                                change_env child_celds dirty_celds obst_celds empty_celds robot_celds playpen n m 0 rand_gen

                 in do
                     putStrLn ("\n Changing env")
                     show_env playpen robot_celds new_child_celds new_dirty_celds new_obst_celds new_empty_celds n m
                     simulation new_child_celds robot_celds new_dirty_celds new_empty_celds new_obst_celds playpen t (turn + 1) turns_number 0 rand_gen n m robot_smart dirty_number sim all_sim percent_clean

            else simulation child_celds robot_celds dirty_celds empty_celds obst_celds playpen t (turn + 1) turns_number 0 rand_gen n m robot_smart dirty_number sim all_sim percent_clean

    | otherwise = 
        let robot = robot_celds !! robot_index 
            (new_child_celds, new_dirty_celds, new_robot_celds, new_empty_celds, new_rand_gen) = 
                            robot_smart robot child_celds empty_celds dirty_celds playpen robot_celds rand_gen n m
            new_robot = new_robot_celds !! robot_index 

        in do
            putStrLn("-----------------------------------------------------------------------------------------------------")
            let name_robot = name robot
            let robot_row = row $ position robot
            let robot_col = col $ position robot
            let new_robot_row = row $ position new_robot
            let new_robot_col = col $ position new_robot

            putStrLn("\nMove " ++ (name_robot) ++ " from " ++ "(" ++ (show robot_row) ++ "," ++ (show robot_col) ++ ")" ++  " to " ++ "(" ++ (show new_robot_row) ++ "," ++ (show new_robot_col) ++ ")")
            show_env playpen new_robot_celds new_child_celds new_dirty_celds obst_celds new_empty_celds n m
            putStrLn("-----------------------------------------------------------------------------------------------------")
            simulation new_child_celds new_robot_celds new_dirty_celds new_empty_celds obst_celds playpen t turn turns_number (robot_index +1) new_rand_gen n m robot_smart dirty_number sim all_sim percent_clean



create_env :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> IO()
create_env n m robots_number childrens_number dirty_number obst_number t turns_number sim all_sim percent_clean
    | sim == (all_sim + 1) = let 
                            average_clean = div (sum percent_clean) (length percent_clean)

                        in  do

                            putStrLn("\n\n-----------------------------------------------------------------------------------------------------")
                            putStrLn("FINAL RESULTS")
                            putStrLn("-----------------------------------------------------------------------------------------------------")

                            write_in_txt("-----------------------------------------------------------------------------------------------------")
                            write_in_txt ("\nFINAL RESULTS")

                            if average_clean < 60
                            then do
                                putStrLn ("Environment dirty")
                                write_in_txt ("Environment dirty")
                            else do
                                putStrLn ("Environment clean")
                                write_in_txt ("Environment clean")

                            putStrLn ("Clean Percent Average: " ++ (show average_clean) ++ "%")
                            write_in_txt ("Clean Percent Average: " ++ (show average_clean) ++ "%" ++ "\n\n")
                            write_in_txt("_____________________________________________________________________________________________________\n\n")

    | otherwise = do
                let empty_celds = create_empty_celds n m

               
                rand_gen <- newStdGen
                rand_gen' <- newStdGen
                let playpen_celds = create_playpen childrens_number n m rand_gen rand_gen'
                let playpen = add_type_elements "Playpen" childrens_number playpen_celds rand_gen

                let aux = update_celds playpen_celds empty_celds
                let empty_celds = aux 

                
                rand_gen <- newStdGen
                let obst_celds = add_type_elements "Obstacle" obst_number empty_celds rand_gen

                let aux = update_celds (get_celds_elements obst_celds) empty_celds
                let empty_celds = aux


                rand_gen <- newStdGen
                let dirty_celds = add_type_elements "Dirty" dirty_number empty_celds rand_gen

                let aux = update_celds (get_celds_elements dirty_celds) empty_celds
                let empty_celds = aux


                rand_gen <- newStdGen
                let child_celds = add_type_elements "Child" childrens_number empty_celds rand_gen

                let aux = update_celds (get_celds_elements child_celds) empty_celds
                let empty_celds = aux

                

                rand_gen <- newStdGen
                let robot_celds = add_type_elements "Robot free" robots_number empty_celds rand_gen

                let aux = update_celds (get_celds_elements robot_celds) empty_celds
                let empty_celds = aux

                

                rand_gen <- newStdGen
                show_env playpen robot_celds child_celds dirty_celds obst_celds empty_celds n m
                simulation child_celds robot_celds dirty_celds empty_celds obst_celds playpen t 1 turns_number 1 rand_gen n m robot_smart dirty_number sim all_sim percent_clean


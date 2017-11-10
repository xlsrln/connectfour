module Main where

import qualified Connectfour
import Graphics.UI.WX
import Data.List (intersperse)


-- this is the ugliest UI ever written. 
-- everything is hardcoded and the text isn't monospaced and the buttons are enormous. 
-- no colours.

main :: IO ()
main = start gui

initboard = Connectfour.initialboard

gui :: IO ()
gui = do
        bv <- variable [ value := initboard]
        board <- get bv value

        f <- frame [ text := "Event Handling" ]
        b1 <- button f [ text := "1" ]
        b2 <- button f [ text := "2" ]
        b3 <- button f [ text := "3" ]
        b4 <- button f [ text := "4" ]
        b5 <- button f [ text := "5" ]
        b6 <- button f [ text := "6" ]
        
        st <- staticText f [ text := "" ]
        
        -- s1 <- staticText f [ text := intersperse '  ' $ head (Connectfour.rows board)]
        -- s2 <- staticText f [ text := intersperse '  ' $ head $ tail (Connectfour.rows board)]
        -- s3 <- staticText f [ text := intersperse '  ' $ head $ tail $ tail (Connectfour.rows board)]
        -- s4 <- staticText f [ text := intersperse '  ' $ head $ tail $ tail $ tail (Connectfour.rows board)]
        -- s5 <- staticText f [ text := intersperse '  ' $ head $ tail $ tail $ tail $ tail (Connectfour.rows board)]
        -- s6 <- staticText f [ text := intersperse '  ' $ head $ tail $ tail $ tail $ tail $ tail (Connectfour.rows board)]
        
        s1 <- staticText f [ text := head (Connectfour.rows board)]
        s2 <- staticText f [ text := head $ tail (Connectfour.rows board)]
        s3 <- staticText f [ text := head $ tail $ tail (Connectfour.rows board)]
        s4 <- staticText f [ text := head $ tail $ tail $ tail (Connectfour.rows board)]
        s5 <- staticText f [ text := head $ tail $ tail $ tail $ tail (Connectfour.rows board)]
        s6 <- staticText f [ text := head $ tail $ tail $ tail $ tail $ tail (Connectfour.rows board)]
        
        set s1 [font := fontFixed]
        set s2 [font := fontFixed]
        set s3 [font := fontFixed]
        set s4 [font := fontFixed]
        set s5 [font := fontFixed]
        set s6 [font := fontFixed]
        
        
        let wongame s = do
            set st [ text := s ++ " won "]
            set b1 [ on command := putStr ""]
            set b2 [ on command := putStr ""]
            set b3 [ on command := putStr ""]
            set b4 [ on command := putStr ""]
            set b5 [ on command := putStr ""]
            set b6 [ on command := putStr ""]
        
        let test = putStrLn "test"
        let up n b s = do
            let ns = Connectfour.nxt s
            let tb = Connectfour.putCoin s n b
            
            if tb == b then print "" else do
                -- putStrLn ns
                set bv [ value := tb]
                set s1 [ text := concat (concat tb ) ]
                -- putStrLn $ Connectfour.status tb
                set b1 [ on command := up 1 tb ns]
                set b2 [ on command := up 2 tb ns]
                set b3 [ on command := up 3 tb ns]
                set b4 [ on command := up 4 tb ns]
                set b5 [ on command := up 5 tb ns]
                set b6 [ on command := up 6 tb ns]
                
                set s1 [ text := head (Connectfour.rows tb)]
                set s2 [ text := head $ tail (Connectfour.rows tb)]
                set s3 [ text := head $ tail $ tail (Connectfour.rows tb)]
                set s4 [ text := head $ tail $ tail $ tail (Connectfour.rows tb)]
                set s5 [ text := head $ tail $ tail $ tail $ tail (Connectfour.rows tb)]
                set s6 [ text := head $ tail $ tail $ tail $ tail $ tail (Connectfour.rows tb)]
                
                let ws = Connectfour.status tb
                if ws == s then wongame ("player " ++ s) else putStr ""
                if ws == "y" then wongame "noone" else putStr ""
        
        set b1 [ on command := up 1 board "x"]
        set b2 [ on command := up 2 board "x"]
        set b3 [ on command := up 3 board "x"]
        set b4 [ on command := up 4 board "x"]
        set b5 [ on command := up 5 board "x"]
        set b6 [ on command := up 6 board "x"]

        let disp = floatCenter $ column 5 [widget st, widget s6, widget s5, widget s4, widget s3, widget s2, widget s1] 
        let thing = column 5 [floatCenter $ row 10 [ widget b1, widget b2, widget b3, 
                                                                            widget b4, widget b5, widget b6] ]
        
        set f [ layout := column 1 [disp, thing]]
        putStrLn $ concat $ concat board
        

-- col n board = Connectfour.putCoin "x" n board



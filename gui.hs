module Main where

import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do
        f <- frame [ text := "Event Handling" ]
        st <- staticText f [ text := "You haven\'t clicked the button yet." ]
        b1 <- button f [ text := "Click me!"
                    , col 1
                    ]
        b2 <- button f [ text := "Click me!"
                    , col 2
                    ]
        b3 <- button f [ text := "Click me!"
                    , col 3
                    ]
        b4 <- button f [ text := "Click me!"
                    , col 4
                    ]
        b5 <- button f [ text := "Click me!"
                    , col 5
                    ]
        set f [ layout := column 25 [ widget st, widget b1 ] ]

col n = putStrLn show n
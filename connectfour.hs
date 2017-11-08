import Data.List

nxt "x" = "o"
nxt "o" = "x"

--board stuff
vsize = 6
hsize = 7
emptyrow = replicate hsize []
initialboard = map (replicate vsize) $ replicate hsize "-"

main = do turn "x" initialboard     
        
--main action, asks for input and does the stuff


doStuff s board = do
        column <- read <$> getLine :: IO Int
        let newboard = putCoin s column board
        if newboard == board 
                then do putStr "wrong input try again: "
                        doStuff s board
                else return newboard

turn s board = do 
        putStrLn " "
        putStr $ "player " ++ s ++ " turn, enter column: "
        
        let new = doStuff s board
        newboard <- new
        
        putStrLn " "
        printBoard $ map reverse newboard
        case status newboard of
            "-" -> turn (nxt s) newboard
            c -> do putStrLn $ "player " ++ c ++ " won"
                    putStrLn " "

--main printing function, prints the current board as rows of strings
printBoard :: [[String]] -> IO ()
printBoard board = do
                    let rest = removeRow board
                    putStrLn $ printRow board
                    if rest == emptyrow then return ()
                        else printBoard rest

--the hard part, determining if someone won or not
status board  
    | listOr $ map (contains "xxxx") $ checklist board = "x"
    | listOr $ map (contains "oooo") $ checklist board = "o"
    | otherwise                                        = "-"

--which uses these logical operations
listOr :: [Bool] -> Bool
listOr = foldr (||) False

contains [] ys = True
contains xs [] = False
contains (x:xs) (y:ys) = (x == y) && contains xs ys || contains (x:xs) ys

--and the following functions
checklist board = concat [rows board, columns board, diagonals board]

--extract all columns to strings
columns :: [[String]] -> [String]
columns board = map concat board

--extract all rows to strings
rows :: [[String]] -> [String]
rows [] = []
rows board = row board : rows (removeRow board) where
    row board = concat [x | (x:xs) <- board ]

--extract all diagonals to strings -- warning for messy code
diagonal n (x:rest) = if n<(length x) then (x !! n) : diagonal (n+1) rest else []
diagonal _ [] = []

--finds middle diagonal and all upper triangular ones
diagonals1 :: [[String]] -> [String]
diagonals1 [] = []
diagonals1 board = (concat $ diagonal 0 board) : diagonals1 (removeRow board)

-- concats diagonals1 with diagonals1 of the transposed board
diagonals2 :: [[String]] -> [String]
diagonals2 board = concat $ (diagonals1 board):[diagonals1 (transpose board)]

-- a total of four calls to diagonals1 is concatenated, this time with/without reverse
diagonals board = concat $ diagonals2 board : [diagonals2 (reverse board)]

--other functions
printRow :: [[String]] -> String
printRow board = concat [x | (x:xs) <- board]

removeRow :: [[String]] -> [[String]]
removeRow board = [xs | (x:xs) <- board]

putCoin :: String -> Int -> [[[Char]]] -> [[[Char]]]
putCoin s 1 (col:cols) = putCoinCol s col : cols
putCoin s n b@(col:cols) = if n > hsize then b else col : putCoin s (n-1) cols

putCoinCol :: [Char] -> [[Char]] -> [[Char]]
putCoinCol s ("-":rs) = (s:rs)
putCoinCol s (r:rs) = r : putCoinCol s rs
putCoinCol s [] = []
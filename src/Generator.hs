module Generator where

import System.Random
import Types


-- | Given a seed returns a list of n integer randomly generated
--
generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-9


-- | Given a seed returns an integer randomly generated
--
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ generateRandoms 1 seed


-- Converssta list into a list of list of size n
--
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | Converts an integer number into a Peca
-- 3 <=> Comida Grangre
-- 0 <= n < 7 <=> Comida Pequena
-- 7 < n <= 9 <=> Parede
--
convertePeca :: Int -> Piece    
convertePeca p
 |p == 3 = Food Big
 |0 <= p && p< 70 = Food Little
 |70 <= p && p <= 99 = Wall


-- | Converts a Corredor to a string
--
printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (x:xs) = show x ++ printCorridor xs




-- | Converts a list of integers into a Corredor
--
converteCorredor :: [Int] -> Corridor
converteCorredor [] = []
converteCorredor (x:xs) = convertePeca x : converteCorredor xs


-- | Converts a list of lists of integers into a Labirinto
--
converteLabirinto :: [[Int]] -> Maze
converteLabirinto [] = []
converteLabirinto (x:xs) = converteCorredor x : converteLabirinto xs


geraLabirinto :: Int -> Int -> Int -> Maze
geraLabirinto x y s =
                 let random_nrs = generateRandoms (x*y) s
                 in converteLabirinto $ subLista x random_nrs

imprimeLabirinto :: Maze -> IO ()
imprimeLabirinto l = do putStrLn ( printMaze ( l ))


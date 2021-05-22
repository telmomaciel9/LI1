module Testes where

import Tarefa1
import Tarefa2
import Tarefa3
import Types

testesTarefa1 :: Int -> Int -> Int -> Maze
testesTarefa1 x y z = generateMaze x y z

testesTarefa11= generateMaze 15 15 9
testesTarefa12= generateMaze 20 34 1
testesTarefa13= generateMaze 44 19 4
testesTarefa14= generateMaze 23 22 10
testesTarefa15= generateMaze 19 17 5
testesTarefa16= generateMaze 31 32 33



testesTarefa2 :: Play -> State -> State
testesTarefa2 (Move id ori) (State m ms l) = play (Move id ori) (State m ms l)

testesTarefa21= play (Move 0 D) (State (generateMaze 15 15 9) [Pacman (PacState (0, (1, 2), 1, L, 0, 3) 0 Open Mega),Ghost (GhoState (2, (1, 1), 1, R, 0, 3) Dead)] 1)
testesTarefa22= play (Move 0 U) (State (generateMaze 20 34 1) [Pacman (PacState (0, (1, 2), 1, L, 0, 3) 0 Open Mega),Ghost (GhoState (2, (1, 1), 1, R, 0, 3) Dead)] 2)
testesTarefa23= play (Move 0 R) (State (generateMaze 44 19 4) [Pacman (PacState (0, (1, 2), 1, L, 0, 3) 0 Open Mega),Ghost (GhoState (2, (1, 1), 1, R, 0, 3) Dead)] 3)
testesTarefa24= play (Move 0 R) (State (generateMaze 23 22 10) [Pacman (PacState (0, (1, 2), 1, L, 0, 3) 0 Open Mega),Ghost (GhoState (2, (1, 1), 1, R, 0, 3) Dead)] 4)
testesTarefa25= play (Move 0 L) (State (generateMaze 19 17 5) [Pacman (PacState (0, (1, 2), 1, L, 0, 3) 0 Open Mega),Ghost (GhoState (2, (1, 1), 1, R, 0, 3) Dead)] 5)
testesTarefa26= play (Move 0 R) (State (generateMaze 31 32 33) [Pacman (PacState (0, (1, 2), 1, L, 0, 3) 0 Open Mega),Ghost (GhoState (2, (1, 1), 1, R, 0, 3) Dead)] 6)



testesTarefa3 :: Maze -> Instructions
testesTarefa3 m = compactMaze m 

testesTarefa31= compactMaze (generateMaze 15 15 9)
testesTarefa32= compactMaze (generateMaze 20 34 1)
testesTarefa33= compactMaze (generateMaze 44 19 4)
testesTarefa34= compactMaze (generateMaze 23 22 10)
testesTarefa35= compactMaze (generateMaze 19 17 5)
testesTarefa36= compactMaze (generateMaze 31 32 33)
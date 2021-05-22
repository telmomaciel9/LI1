{-|
Module : Tarefa2
Description : Módulo Haskell com funções necessárias para implementar uma jogada.
Copyright : Telmo Maciel <a96569@alunos.uminho.pt>;
            Tiago Soares <a97381@alunos.uminho.pt>

= Introdução

 A tarefa 2 foi uma tarefa essencial visto que nos permitiu estar pela primeira vez em contacto com as
ideias fundamentais do jogo do Pacman.

= Objetivos

 Nesta tarefa o objetivo era criar as mecânicas essenciais para o funcionamento do jogo, sendo que foi
uma tarefa que serviu de alicerce para as seguintes. O nosso modo de agir centrou-se nas dinâmicas de
ação entre as diferentes entidades. Basicamente era preciso definir as ideias do jogo, como por exemplo o
que acontece quando o pacman embate com uma parede, fantasma ou comida. Para isso, tivemos de definir
todos esses casos, assim como ideias ainda mais simples, como a movimentação do jogador. No final, foi
uma questão de agregar todos estes detalhes numa função final, sendo que foi expecialmente importante 
fazer testes, para saber como iam progredindo as jogadas implementadas.

= Discussão e Conclusão

 Foi provavelmente a mais trabalhosa das três tarefas iniciais. Seguimos uma abordagem linear e focamos
na criação de condições para determinar o que acontecia em várias situações diferentes.

-}
module Tarefa2 where

import Types 
import FileUtils
import Tarefa1




{-| Como começo, criámos a função que dado um labirinto, um jogador do tipo Pacman e a orientação que pretendemos seguir, altera as coordenadas do jogador para que a sua movimentação seja implementada.

@
movePlayer :: Maze -> Player -> Orientation -> Player
movePlayer (h2:t2) (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) a
     | y==R && a==R && (b,c) == (b, ((length h2)-1)) = (Pacman (PacState(i,(b,0), x, R,z,l)h (changeMouth f) j)) --Dois casos possiveis de tuneis
     | y==L && a==L && (b,c) == (b,0) = (Pacman (PacState(i,(b, ((length h2)-1)), x, L,z,l)h (changeMouth f) j))
     | y== L && a == L && getPiece (b,c-1) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, L,z,l)h (changeMouth f) j)) --Caso a peça para onde transita seja parede
     | y== R && a == R && getPiece (b,c+1) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, R,z,l)h (changeMouth f) j))
     | y== U && a == U && getPiece (b-1,c) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, U,z,l)h (changeMouth f) j))
     | y== D && a == D && getPiece (b+1,c) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, D,z,l)h (changeMouth f) j)) 
     | y== L && a == L && getPiece (b,c-1) (h2:t2) /= Wall =(Pacman (PacState(i,(b,c-1), x, L,z,l)h (changeMouth f) j)) --Caso a peça para onde transita não seja parede
     | y== R && a == R && getPiece (b,c+1) (h2:t2) /= Wall =(Pacman (PacState(i,(b,c+1), x, R,z,l)h (changeMouth f) j))
     | y== U && a == U && getPiece (b-1,c) (h2:t2) /= Wall =(Pacman (PacState(i,(b-1,c), x, U,z,l)h (changeMouth f) j))
     | y== D && a == D && getPiece (b+1,c) (h2:t2) /= Wall =(Pacman (PacState(i,(b+1,c), x, D,z,l)h (changeMouth f) j)) 
     | y/=a  =(Pacman (PacState(i,(b,c), x, a,z,l)h (changeMouth f) j)) 
@

-}
--Função para o movimento simples de um fantasma
movePlayer :: Maze -> Player -> Orientation -> Player
movePlayer (h2:t2) (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) a
     | y==R && a==R && (b,c) == (b, ((length h2)-1)) = (Pacman (PacState(i,(b,0), x, R,z,l)h (changeMouth f) j)) --Dois casos possiveis de tuneis
     | y==L && a==L && (b,c) == (b,0) = (Pacman (PacState(i,(b, ((length h2)-1)), x, L,z,l)h (changeMouth f) j))
     | y== L && a == L && getPiece (b,c-1) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, L,z,l)h (changeMouth f) j)) --Caso a peça para onde transita seja parede
     | y== R && a == R && getPiece (b,c+1) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, R,z,l)h (changeMouth f) j))
     | y== U && a == U && getPiece (b-1,c) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, U,z,l)h (changeMouth f) j))
     | y== D && a == D && getPiece (b+1,c) (h2:t2) == Wall =(Pacman (PacState(i,(b,c), x, D,z,l)h (changeMouth f) j)) 
     | y== L && a == L && getPiece (b,c-1) (h2:t2) /= Wall =(Pacman (PacState(i,(b,c-1), x, L,z,l)h (changeMouth f) j)) --Caso a peça para onde transita não seja parede
     | y== R && a == R && getPiece (b,c+1) (h2:t2) /= Wall =(Pacman (PacState(i,(b,c+1), x, R,z,l)h (changeMouth f) j))
     | y== U && a == U && getPiece (b-1,c) (h2:t2) /= Wall =(Pacman (PacState(i,(b-1,c), x, U,z,l)h (changeMouth f) j))
     | y== D && a == D && getPiece (b+1,c) (h2:t2) /= Wall =(Pacman (PacState(i,(b+1,c), x, D,z,l)h (changeMouth f) j)) 
     | y/=a  =(Pacman (PacState(i,(b,c), x, a,z,l)h (changeMouth f) j)) 

{-| As duas próximas funções recebem o jogador do tipo Pacman e o labirinto e mudam a sua pontuação consoante a peça que se encontra na mesma posição que o jogador.

@
updateWithCoords :: Player -> Maze -> Player
updateWithCoords a@(Pacman (PacState (i,(b,c), x, y,z,l)h f j)) m 
    | getPiece (b,c) m == Food Little = updatePlayer EatLittle a
    | getPiece (b,c) m == Food Big = updatePlayer EatBig a
    | otherwise= if h==0 then (Pacman (PacState(i,(b,c), x, y,z,l)h f Normal)) 
                         else (Pacman (PacState(i,(b,c), x, y,z,l)h f j)) 


updatePlayer :: PlayAction -> Player -> Player
updatePlayer p (Pacman (PacState(i,(b,c), x, y,z,l)h f j))  = case p of EatLittle ->if h==0 then (Pacman (PacState(i,(b,c), x, y,(z+1),l)h f Normal)) 
                                                                                            else (Pacman (PacState(i,(b,c), x, y,(z+1),l)h f j)) 
                                                                        EatBig -> (Pacman (PacState(i,(b,c), x, y,(z+5),l)10 f Mega))
@

-}

updateWithCoords :: Player -> Maze -> Player
updateWithCoords a@(Pacman (PacState (i,(b,c), x, y,z,l)h f j)) m 
    | getPiece (b,c) m == Food Little = updatePlayer EatLittle a
    | getPiece (b,c) m == Food Big = updatePlayer EatBig a
    | otherwise= if h==0 then (Pacman (PacState(i,(b,c), x, y,z,l)h f Normal)) 
                         else (Pacman (PacState(i,(b,c), x, y,z,l)h f j)) 

updatePlayer :: PlayAction -> Player -> Player
updatePlayer p (Pacman (PacState(i,(b,c), x, y,z,l)h f j))  = case p of EatLittle -> if h==0 then (Pacman (PacState(i,(b,c), x, y,(z+1),l)h f Normal)) 
                                                                                             else (Pacman (PacState(i,(b,c), x, y,(z+1),l)h f j)) 
                                                                        EatBig -> (Pacman (PacState(i,(b,c), x, y,(z+5),l)10 f Mega))


{-| Estas 3 funções testam todos os casos em que o Pacman embate com um fantasma. Resultam no Pacman (no caso da função 'changePacmanPlayer') e nos fantasmas ('changeGhostPlayer').

A função 'ghostIfPacmanIsMega' apenas transforma os fantasmas do estado Alive para Dead caso o pacman esteja em estado Mega, caso contrário, permanece tudo igual.

@
changeGhostPlayer :: Player -> [Player] -> Maze -> [Player]
changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) [] maze = []
changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):hs) maze
                     | b==b1 && c==c1 && d==Dead = case (odd (length maze)) of True -> (Ghost(GhoState (i1,(div(length (maze)) 2,(div (length (head maze)) 2)),x1,y1,z1,l1) Alive)): changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs maze
                                                                               False -> (Ghost(GhoState (i1,((div(length (maze)) 2)-1,(div (length (head maze)) 2)),x1,y1,z1,l1) Alive)): changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs maze
                     | otherwise = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)): changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs maze

ghostIfPacmanIsMega :: Player -> [Player] -> [Player]
ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) [] = []
ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):hs)
         |j==Mega && h==10 =(Ghost(GhoState (i1,(b1,c1), x1/2.0, y1,z1,l1) Dead)): ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f Mega)) hs
         |j==Mega =(Ghost(GhoState (i1,(b1,c1), x1/2.0, y1,z1,l1) d)): ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs
         |j==Normal = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)): ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f Normal)) hs

changePacmanPlayer :: Player -> [Player] -> Player
changePacmanPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) []  = (Pacman(PacState (i,(b,c), x, y,z,l) h f j))
changePacmanPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):xs)
                                          | b==b1 && c==c1 = case d of Dead -> (Pacman(PacState (i,(b,c), x, y,z+10,l) h f Mega))
                                                                       Alive -> case l of 0 -> (Pacman(PacState (i,(b,c), x, y,z,l) h f Dying))
                                                                                          _ -> (Pacman(PacState (i,(b,c), x, y,z,l-1) h f j)) 
                                          | otherwise= changePacmanPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) xs 
@

-}

-- Retorna os fantasmas.
changeGhostPlayer :: Player -> [Player] -> Maze -> [Player]
changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) [] maze = []
changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):hs) maze
                     | b==b1 && c==c1 && d==Dead = case (odd (length maze)) of True -> (Ghost(GhoState (i1,(div(length (maze)) 2,(div (length (head maze)) 2)),x1,y1,z1,l1) Alive)): changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs maze
                                                                               False -> (Ghost(GhoState (i1,((div(length (maze)) 2)-1,(div (length (head maze)) 2)),x1,y1,z1,l1) Alive)): changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs maze
                     | otherwise = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)): changeGhostPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs maze

ghostIfPacmanIsMega :: Player -> [Player] -> [Player]
ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) [] = []
ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):hs)
         |j==Mega && h==10 =(Ghost(GhoState (i1,(b1,c1), x1/2.0, (rotate180Degrees y1),z1,l1) Dead)): ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f Mega)) hs
         |j==Mega =(Ghost(GhoState (i1,(b1,c1), x1/2.0, y1,z1,l1) d)): ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) hs
         |j==Normal = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)): ghostIfPacmanIsMega (Pacman(PacState (i,(b,c), x, y,z,l) h f Normal)) hs

--  Devolve o pacman
changePacmanPlayer :: Player -> [Player] -> Player
changePacmanPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) []  = (Pacman(PacState (i,(b,c), x, y,z,l) h f j))
changePacmanPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):xs)
                                          | b==b1 && c==c1 = case d of Dead -> (Pacman(PacState (i,(b,c), x, y,z+10,l) h f Mega))
                                                                       Alive -> case l of 0 -> (Pacman(PacState (i,(b,c), x, y,z,l) h f Dying))
                                                                                          _ -> (Pacman(PacState (i,(b,c), x, y,z,l-1) h f j)) 
                                          | otherwise= changePacmanPlayer (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) xs  

{-| Esta função recebe um jogador do tipo Pacman e um labirinto, devolvendo um novo labirinto, com a peça por onde o Pacman transitou na forma de "Empty".

@
updateMaze :: Player -> Maze -> Maze
updateMaze (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) m = replaceElemInMaze (b,c) Empty m
@

-}

updateMaze :: Player -> Maze -> Maze
updateMaze (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) m = replaceElemInMaze (b,c) Empty m

{-| Esta função foi criada para ser possível fazer a movimentação de um fantasma.

@
moveGhost :: Maze -> Player -> Orientation -> Player
moveGhost (h2:t2) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)) Null = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d))
moveGhost (h2:t2) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)) a
     | y1== L && a == L && getPiece (b1,c1-1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, L,z1,l1) d))--Caso a peça para onde transita seja parede
     | y1== R && a == R && getPiece (b1,c1+1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, R,z1,l1) d))
     | y1== U && a == U && getPiece (b1-1,c1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, U,z1,l1) d))
     | y1== D && a == D && getPiece (b1+1,c1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, D,z1,l1) d))
     | y1== L && a == L && getPiece (b1,c1-1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1,c1-1), x1, L,z1,l1) d)) --Caso a peça para onde transita não seja parede
     | y1== R && a == R && getPiece (b1,c1+1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1,c1+1), x1, R,z1,l1) d))
     | y1== U && a == U && getPiece (b1-1,c1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1-1,c1), x1, U,z1,l1) d))
     | y1== D && a == D && getPiece (b1+1,c1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1+1,c1), x1, D,z1,l1) d))
     | y1/=a  =(Ghost(GhoState (i1,(b1,c1), x1, a,z1,l1) d))
@

-}

moveGhost :: Maze -> Player -> Orientation -> Player
moveGhost (h2:t2) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)) Null = (Ghost(GhoState (i1,(b1,c1), x1, Null,z1,l1) d))
moveGhost (h2:t2) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)) a
     | y1== L && a == L && getPiece (b1,c1-1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, L,z1,l1) d))--Caso a peça para onde transita seja parede
     | y1== R && a == R && getPiece (b1,c1+1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, R,z1,l1) d))
     | y1== U && a == U && getPiece (b1-1,c1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, U,z1,l1) d))
     | y1== D && a == D && getPiece (b1+1,c1) (h2:t2) == Wall =(Ghost(GhoState (i1,(b1,c1), x1, D,z1,l1) d))
     | y1== L && a == L && getPiece (b1,c1-1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1,c1-1), x1, L,z1,l1) d)) --Caso a peça para onde transita não seja parede
     | y1== R && a == R && getPiece (b1,c1+1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1,c1+1), x1, R,z1,l1) d))
     | y1== U && a == U && getPiece (b1-1,c1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1-1,c1), x1, U,z1,l1) d))
     | y1== D && a == D && getPiece (b1+1,c1) (h2:t2) /= Wall =(Ghost(GhoState (i1,(b1+1,c1), x1, D,z1,l1) d))
     | y1/=a  =(Ghost(GhoState (i1,(b1,c1), x1, a,z1,l1) d))

{-| A função 'play' é a função final. Dada uma jogada do tipo (Move ID Orientation) e um State, devolve um novo State com as transformações necessárias realizadas.

@
play :: Play -> State -> State
play (Move n o) s@(State m ms l) 
  | isPacman (getPlayer n ms)== True && getPacmanMode (getPlayer n ms)== Normal = (State (updateMaze (movePlayer m (whereIsPacman ms) o) m) ((changePacmanPlayer (updateWithCoords (movePlayer m (whereIsPacman ms) o)m) (whereAreGhosts ms) ):(changeGhostPlayer (updateWithCoords (movePlayer m (whereIsPacman ms) o)m) (ghostIfPacmanIsMega (updateWithCoords (movePlayer m (whereIsPacman ms) o)m)  (whereAreGhosts ms)) m))  l) 
  | isPacman (getPlayer n ms)== True && getPacmanMode (getPlayer n ms)== Mega =  (State (updateMaze (movePlayer m (whereIsPacman ms') o) m) ((changePacmanPlayer (updateWithCoords (movePlayer m (whereIsPacman ms') o)m) (whereAreGhosts ms') ):(changeGhostPlayer (updateWithCoords (movePlayer m (whereIsPacman ms') o)m) (ghostIfPacmanIsMega (updateWithCoords (movePlayer m (whereIsPacman ms') o)m)  (whereAreGhosts ms')) m))  l) 
  | otherwise= replacePlayer (moveGhost m (getPlayerFromList n s) o) s 
     where reduceTimePac (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) = (Pacman (PacState (i,(b,c), x, y,z,l) (h-0.25) f j))
           ms'= (replacePlayerAux (reduceTimePac (getPlayer n ms)) ms)
@
-}

--Função final
play :: Play -> State -> State
play (Move n o) s@(State m ms l) 
  | isPacman (getPlayer n ms)== True && getPacmanMode (getPlayer n ms)== Normal = (State (updateMaze (movePlayer m (whereIsPacman ms) o) m) ((changePacmanPlayer (updateWithCoords (movePlayer m (whereIsPacman ms) o)m) (whereAreGhosts ms) ):(changeGhostPlayer (updateWithCoords (movePlayer m (whereIsPacman ms) o)m) (ghostIfPacmanIsMega (updateWithCoords (movePlayer m (whereIsPacman ms) o)m)  (whereAreGhosts ms)) m))  l) 
  | isPacman (getPlayer n ms)== True && getPacmanMode (getPlayer n ms)== Mega =  (State (updateMaze (movePlayer m (whereIsPacman ms') o) m) ((changePacmanPlayer (updateWithCoords (movePlayer m (whereIsPacman ms') o)m) (whereAreGhosts ms') ):(changeGhostPlayer (updateWithCoords (movePlayer m (whereIsPacman ms') o)m) (ghostIfPacmanIsMega (updateWithCoords (movePlayer m (whereIsPacman ms') o)m)  (whereAreGhosts ms')) m))  l) 
  | otherwise= replacePlayer (moveGhost m (getPlayerFromList n s) o) s 
     where reduceTimePac (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) = (Pacman (PacState (i,(b,c), x, y,z,l) (h-0.25) f j))
           ms'= (replacePlayerAux (reduceTimePac (getPlayer n ms)) ms)


{-| A função 'play2' foi só criada para apoio à funçao final, esta aceita várias jogada seguidas numa lista de orientações e aplica-as uma a uma.

@
play2 :: Int -> [Orientation] -> State -> State
play2 id []  s =s
play2 id (x:xs) s
   | x== R = play2 id xs (play (Move id R) s)
   | x== D = play2 id xs (play (Move id D) s)
   | x== L = play2 id xs (play (Move id L) s)
   | x== U = play2 id xs (play (Move id U) s)@
-}

play2 :: Int -> [Orientation] -> State -> State
play2 id []  s =s
play2 id (x:xs) s
   | x== R = play2 id xs (play (Move id R) s)
   | x== D = play2 id xs (play (Move id D) s)
   | x== L = play2 id xs (play (Move id L) s)
   | x== U = play2 id xs (play (Move id U) s)
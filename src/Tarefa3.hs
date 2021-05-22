{-|
Module : Tarefa3
Description : Módulo Haskell com funções para a compactação do labirinto.
Copyright : Telmo Maciel <a96569@alunos.uminho.pt>;
            Tiago Soares <a97381@alunos.uminho.pt>

= Introdução

 Nesta tarefa tínhamos de compactar as intruções de forma a criar labirintos de uma forma fragmentada.

= Objetivos

 Começamos por "organizar" o formato de labirinto a que estavámos habituados de diferente forma. Após
isso, conseguimos agrupar "peças" do labirinto iguais, compactando todo o processo. Depois, foi uma
questão de manter o raciocínio e continuar o agrupamento, chegando a agrupar vários corredores iguais,
por exemplo.

= Discussão e Conclusão

 Na nossa resolução da tarefa o essencial foi perceber a mecânica que estava a ser pedida. Assim que
os princípios fundamentais foram cobertos, fomos expandindo a forma de pensar e resolvendo de forma
simples a compactação.


Ao longo deste módulo é usado o mesmo exemplo, para uma interpretação mais fácil das funções.
-}
module Tarefa3 where

import Types
import Tarefa1
import System.Random


{-| De modo a iniciar a tarefa, usamos um labirinto e transformamos todas as suas peças num tuplo (Int,Piece):

@
convertToIntPiece :: Maze -> [[(Int,Piece)]]
convertToIntPiece [] = []
convertToIntPiece (h:t)=   [aux1Corridor h]  ++ convertToIntPiece t
@


  == Exemplos de utilização:

  >>> convertToIntPiece [[Wall,Wall,Wall,Wall,Wall]]
  [[(1,#),(1,#),(1,#),(1,#),(1,#)]]

  >>> convertToIntPiece [[Wall,Wall,Wall],[Wall,Food Big,Wall],[Wall,Wall,Wall]]
  [[(1,#),(1,#),(1,#)],[(1,#),(1,o),(1,#)],[(1,#),(1,#),(1,#)]]


-}

convertToIntPiece :: Maze -> [[(Int,Piece)]]
convertToIntPiece [] = []
convertToIntPiece (h:t)=   [aux1Corridor h]  ++ convertToIntPiece t

{-| Esta função serve como auxiliar da anterior, apenas recebe um corredor e devolve o mesmo já no formato (Int,Piece).

@
aux1Corridor :: Corridor -> [(Int,Piece)]
aux1Corridor [] = []
aux1Corridor (h:t) 
  | h==Wall = (1,Wall) : aux1Corridor t
  | h==Empty = (1,Empty) : aux1Corridor t
  | h==Food Little = (1,Food Little) : aux1Corridor t
  | h==Food Big = (1,Food Big) : aux1Corridor t
@


  == Exemplos de utilização (no caso do segundo exemplo dado anteriormente):

  >>> aux1Corridor [Wall,Wall,Wall]
  [(1,#),(1,#),(1,#)]

  >>> aux1Corridor [Wall,Food Big,Wall]
  [(1,#),(1,o),(1,#)]

  >>> aux1Corridor [Wall,Wall,Wall]
  [(1,#),(1,#),(1,#)]

-}

aux1Corridor :: Corridor -> [(Int,Piece)]
aux1Corridor [] = []
aux1Corridor (h:t) 
  | h==Wall = (1,Wall) : aux1Corridor t
  | h==Empty = (1,Empty) : aux1Corridor t
  | h==Food Little = (1,Food Little) : aux1Corridor t
  | h==Food Big = (1,Food Big) : aux1Corridor t

{-| Esta função recebe um labirinto e verifica se existem tuplos seguidos iguais dentro de um corredor, nesse caso junta-os.

@
shortCorridor :: [[(Int,Piece)]] -> [[(Int,Piece)]]
shortCorridor [] = []
shortCorridor (h:t) = [auxSameCorridor h] ++ shortCorridor t
@


  == Exemplos de utilização:

  >>> shortCorridor [[(1,Wall),(1,Wall),(1,Wall)],[(1,Wall),(1,Food Big),(1,Wall)],[(1,Wall),(1,Wall),(1,Wall)]]
  [[(3,#)],[(1,#),(1,o),(1,#)],[(3,#)]]


-}

shortCorridor :: [[(Int,Piece)]] -> [[(Int,Piece)]]
shortCorridor [] = []
shortCorridor (h:t) = [auxSameCorridor h] ++ shortCorridor t

{-| A 'auxSameCorridor' funciona apenas como auxiliar à função 'shortCorridor' mas faz o processo corredor a corredor.

@
auxSameCorridor :: [(Int,Piece)] -> [(Int,Piece)]
auxSameCorridor [] = []
auxSameCorridor [(a,b)] = [(a,b)]
auxSameCorridor ((a,b):(c,d):t)
  | b==d = auxSameCorridor ((a+c,d):t)
  | b/=d = (a,b) : auxSameCorridor ((c,d):t)
@


  == Exemplos de utilização:

  >>> shortCorridor [[(1,Wall),(1,Wall),(1,Wall)]]
  [[(3,#)]]

  >>> shortCorridor [[(1,Wall),(1,Food Big),(1,Wall)]]
  [(1,#),(1,o),(1,#)]

-}

auxSameCorridor :: [(Int,Piece)] -> [(Int,Piece)]
auxSameCorridor [] = []
auxSameCorridor [(a,b)] = [(a,b)]
auxSameCorridor ((a,b):(c,d):t)
  | b==d = auxSameCorridor ((a+c,d):t)
  | b/=d = (a,b) : auxSameCorridor ((c,d):t)

{-| Esta função recebe o labirinto dado inicialmente e resulta num conjunto de números inteiros em que, caso haja repetição de corredores, apresenta os mesmos com o indice do primeiro que surgiu. Reparemos nos exemplos mais abaixo:

@
sameIndex :: Maze -> Maze -> [Int]
sameIndex [] _ = []
sameIndex (h:t) m = (checkSameCorridors 0 m h): sameIndex t m 
@


  == Exemplos de utilização:

  >>> sameIndex [[Wall,Wall,Wall],[Wall,Food Big,Wall],[Wall,Wall,Wall]] [[Wall,Wall,Wall],[Wall,Food Big,Wall],[Wall,Wall,Wall]]
  [0,1,0,0,1,0]
  (Neste caso, o segundo 0 significa a repetição do corredor de índice 0)
 

-}

--Devolve os índices dos repetidos.   
sameIndex :: Maze -> Maze -> [Int]
sameIndex [] _ = []
sameIndex (h:t) m = (checkSameCorridors 0 m h): sameIndex t m 

{-| Esta função é a parte essencial da anterior, uma vez que esta, dado um corredor, verifica se este se repete ao longo do labirinto e, no fim, apresenta-os com o mesmo indice.

@
checkSameCorridors :: Int -> Maze -> Corridor -> Int
checkSameCorridors c [] _ = c
checkSameCorridors c (h:t) d
  |auxcheckSame h d = c
  |otherwise= checkSameCorridors (c+1) t d
      where auxcheckSame [] [] = True
            auxcheckSame (h1:t1) (h2:t2)
                |h1==h2 = auxcheckSame t1 t2
                |otherwise= False 
@


  == Exemplos de utilização:

  >>> checkSameCorridors 0 [[Wall,Wall,Wall],[Wall,Food Big,Wall],[Wall,Wall,Wall]] [Wall,Wall,Wall]
  0


-}

--Verifica os corredores que são iguais ao primeiro. 
checkSameCorridors :: Int -> Maze -> Corridor -> Int
checkSameCorridors c [] _ = c
checkSameCorridors c (h:t) d
  |auxcheckSame h d = c
  |otherwise= checkSameCorridors (c+1) t d
      where auxcheckSame [] [] = True
            auxcheckSame (h1:t1) (h2:t2)
                |h1==h2 = auxcheckSame t1 t2
                |otherwise= False 

{-| A função 'convertToInstructions' recebe um contador, o labirinto já no formato de tuplos (Int,Piece) e a lista dos números inteiros obtidos nas funções anteriores e, dado isto, converte o labirinto num conjunto de instruções.


@
convertToInstructions :: Int -> [[(Int,Piece)]] -> [Int] -> Instructions
convertToInstructions _ _ [] = []
convertToInstructions c maze (h:t)
  | c==h = [Instruct (head maze)] ++ convertToInstructions (c+1) (tail maze) t
  | otherwise= Repeat h: convertToInstructions (c+1) (tail maze) t
@


  == Exemplos de utilização:

  >>> convertToInstructions 0 [[(3,Wall)],[(1,Wall),(1,Food Big),(1,Wall)],[(3,Wall)]] [0,1,0]
  [Instruct [(3,#)],Instruct [(1,#),(1,o),(1,#)],Repeat 0]

-}

convertToInstructions :: Int -> [[(Int,Piece)]] -> [Int] -> Instructions
convertToInstructions _ _ [] = []
convertToInstructions c maze (h:t)
  | c==h = [Instruct (head maze)] ++ convertToInstructions (c+1) (tail maze) t
  | otherwise= Repeat h: convertToInstructions (c+1) (tail maze) t

{-| Por fim, resta juntar todas estas funções, para ser possível fazer todos estes passos utlizando apenas uma função ('compactMaze'):


@
compactMaze :: Maze -> Instructions
compactMaze maze = convertToInstructions 0 (shortCorridor (convertToIntPiece maze)) (sameIndex maze maze)
@


  == Exemplos de utilização:

  >>> compactMaze [[Wall,Wall,Wall],[Wall,Food Big,Wall],[Wall,Wall,Wall]]
  [Instruct [(3,#)],Instruct [(1,#),(1,o),(1,#)],Repeat 0]


-}

--Função final
compactMaze :: Maze -> Instructions
compactMaze maze = convertToInstructions 0 (shortCorridor (convertToIntPiece maze)) (sameIndex maze maze)
{- |
Module : Tarefa6
Description : Módulo Haskell com funções necessárias para a criação de um bot.
Copyright : Telmo Maciel <a96569@alunos.uminho.pt>;
            Tiago Soares <a97381@alunos.uminho.pt>

= Introdução

  A tarefa 6 é quase como que um desafio para nós, nesta temos que tentar criar um bot, isto é, um "jogador" capaz 
de jogar sem qualquer tipo de interação humana e também que o mesmo tenha a maior inteligencia possível. 

= Objetivos

  O objetivo desta função não foi mais nada que implementar um robô que conseguisse jogar /Pacman/ automaticamente.

  O nosso pensamento para a realização desta tarefa foi o mesmo que usamos para a tarefa anterior, pegar nas coordenadas
dos vários jogadores e analisar a melhor opção a tomar tendo em conta esse fator. Então, primeiro de tudo precisávamos de
saber qual era o fantasma que se encontrava mais perto do nosso pacman para, posteriormente, pensar no que fazer. Temos 3 
possibilidades diferentes de reação, ou o pacman fugia do fantasma caso ele tivesse em modo Normal, ou o pacman ia atrás 
do fantasma para o tentar comer, ou tinha a possibilidade de se dirigir à comida grande mais próxima, isto é, 
não tendo nenhum fantasma perto dele a colocar em perigo as suas vidas, este dirigia-se à comida mais próxima para, 
posteriormente colocar os fantasmas em modo dead e ir atrás deles para os comer. 
  
  Para tal, após determinar qual o fantasma que se encontra mais perto do nosso pacman, fomos analisar as coordenadas dos
dois jogadores e, depois, determinamos a jogada a efetuar consoante as posições de cada um, isto é, verificavamos as abcissas
e ordenadas e tomávamos uma decisão para onde se ia movimentar o nosso pacman.

  No caso de ele tentar ir comer a comida grande mais próxima tomamos em conta o mesmo raciocínio, fomos obter as coordenadas
da comida no labirinto original e, consequentemente implementamos uma função que analisava as coordenadas do pacman e da 
comida e devolvia um movimento consoante a melhor solução possível.

= Discussão e Conclusão

  Infelizmente, temos noção que o nosso bot não está perto do patamar perfeito, no entanto, tentamos sempre fazer o nosso
melhor para que o mesmo ficasse da forma mais eficaz possivel tendo em conta as nossas aprendizagens e competências. 

  Em conclusão, a realização desta tarefa teve tanta ou mais dificuldade que a tarefa 5, no entanto, fomos conseguindo 
corrigir todos os erros que nos iam aparecendo, conseguimos concluir as nossas funções e, por fim, implementá-la num exemplo
para vê-la a correr.

  Por fim, resta-nos dizer que este trabalho foi muito diferente do que estavamos à espera pois contávamos com coisas mais 
simples, uma vez que nos encontramos apenas nos primeiros meses no mundo da programação. No entanto, foi um trabalho que nos deu
gosto de fazer pois expandimos o nosso conhecimento e abrimos os nossos horizontes em relação ao mundo da programação. 
  Sabemos também que este trabalho é capaz de estar longe do perfeito, no entanto, fizemos sempre o nosso melhor e no final ficamos
orgulhosos de tudo que fizemos. Este trabalho foi um grande desafio para nós já que nos pôs a pensar em todas as soluções e alternativas
das diversas funções que fomos construindo mas, no final, olhamos para trás e começamos a ter noção da vida de um programador e as 
dificuldades que estes mesmos passam.

-}

module Tarefa6 where 

import Types
import Tarefa1 
import Tarefa2

{-| Função que nos dá a distância arredondada às unidades em função de um par de coordenadas.

@
distance :: Coords -> Coords -> Int
distance (x1, y1) (x2, y2) = round (sqrt (x*x + y*y))
      where x = (fromIntegral x1) - (fromIntegral x2)
            y = (fromIntegral y1) - (fromIntegral y2)
@

-}

distance :: Coords -> Coords -> Int
distance (x1, y1) (x2, y2) = round (sqrt (x*x + y*y))
      where x = (fromIntegral x1) - (fromIntegral x2)
            y = (fromIntegral y1) - (fromIntegral y2)

{-| Obtém um par do tipo (/ID/,/Coordenadas/) que nos da o id e as coordenadas do pacman

@
coordsPacman :: [Player] -> (Int,Coords)
coordsPacman ms = (getPlayerID (whereIsPacman ms),(getPlayerCoords (whereIsPacman ms)))
@

-}

coordsPacman :: [Player] -> (Int,Coords)
coordsPacman ms = (getPlayerID (whereIsPacman ms),(getPlayerCoords (whereIsPacman ms)))

{-| Obtém uma lista de pares do tipo (/ID/,/Coordenadas/) que nos da os ids e as coordenadas de todos os fantasmas

@
coordsGhosts :: [Player] -> [(Int,Coords)]
coordsGhosts [] = []
coordsGhosts ms= ((getPlayerID (head (whereAreGhosts ms))),(getPlayerCoords (head (whereAreGhosts ms)))) : coordsGhosts (tail (whereAreGhosts ms))
@

-}

coordsGhosts :: [Player] -> [(Int,Coords)]
coordsGhosts [] = []
coordsGhosts ms= ((getPlayerID (head (whereAreGhosts ms))),(getPlayerCoords (head (whereAreGhosts ms)))) : coordsGhosts (tail (whereAreGhosts ms))

{-| Esta vai pegar nas coordenadas do pacman (do tipo (/ID/,/Coords/)) e no conjunto de coordenadas dos fantasmas (/ID/,/Coords/) e vai devolver um par (/ID/,/Distancia/)
em que o ID vai ser o ID do pacman e a distancia é a distancia do pacman a esse fantasma (arredondada às unidades).

@
getAllDistances :: (Int,Coords) -> [(Int,Coords)] -> [(Int,Int)]
getAllDistances _ [] = []
getAllDistances (a,b) ((h,t):tail) = (h, (distance b t)): getAllDistances (a,b) tail
@

-}

getAllDistances :: (Int,Coords) -> [(Int,Coords)] -> [(Int,Int)]
getAllDistances _ [] = []
getAllDistances (a,b) ((h,t):tail) = (h, (distance b t)): getAllDistances (a,b) tail

{-| Esta função vai pegar no resultado da anterior e vai verificar qual é o fantasma que se encontra mais próximo do nosso jogador, devolvenvo um par do tipo
(/ID/,/Distancia/).

@
getMinimumDistance :: [(Int,Int)] -> (Int,Int)
getMinimumDistance [a]= a
getMinimumDistance ((a,b):(x,y):t)
   | b<y = getMinimumDistance ((a,b):t)
   | otherwise = getMinimumDistance ((x,y):t)
@

-}
--1º determinar o mais proximo
getMinimumDistance :: [(Int,Int)] -> (Int,Int)
getMinimumDistance [a]= a
getMinimumDistance ((a,b):(x,y):t)
   | b<y = getMinimumDistance ((a,b):t)
   | otherwise = getMinimumDistance ((x,y):t)

{-| Esta função apenas nos da um ID recebendo um par (/ID/,/Distancia/).

@
getID :: (Int,Int) -> Int
getID (x,y)= x
@

-}

getID :: (Int,Int) -> Int
getID (x,y)= x

{-| Esta função vai receber um ID do fantasma e vai à lista de jogadores procurar o fantasma.

@
findGhost :: Int -> [Player] -> Player
findGhost id (h:t)
  | id== getPlayerID h = h
  | otherwise= findGhost id t
@
-}

findGhost :: Int -> [Player] -> Player
findGhost id (h:t)
  | id== getPlayerID h = h
  | otherwise= findGhost id t

{-| Esta função vai receber o nosso jogador e o fantasma que se encontra mais próximo dele e, analisando as duas coordenadas
vai devolver a melhor jogada a tomar tendo em conta se o fantasma se encontra no modo Dead ou Alive. Pode também, no caso de 
não se encontrar nenhum fantasma proximo do nosso jogador, chamar a função que nos vai levar atrás de uma comida grande.

@
getNewMove :: Player -> Player -> State -> Play
getNewMove (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Dead)) (State m ms lvl)
         | y==R && getPiece (b,c+1) m == Wall = if b<b1 then (Move i D) else (Move i U) 
         | y==L && getPiece (b,c-1) m == Wall = if b<b1 then (Move i D) else (Move i U) 
         | y==U && getPiece (b-1,c) m == Wall = if c<c1 then (Move i R) else (Move i L) 
         | y==D && getPiece (b+1,c) m == Wall = if c<c1 then (Move i R) else (Move i L) 
         | y==R && getPiece (b,c+1) m == Wall = if b>b1 then (Move i U) else (Move i D) 
         | y==L && getPiece (b,c-1) m == Wall = if b>b1 then (Move i U) else (Move i D) 
         | y==U && getPiece (b-1,c) m == Wall = if c>c1 then (Move i L) else (Move i R) 
         | y==D && getPiece (b+1,c) m == Wall = if c>c1 then (Move i L) else (Move i R) 
         | c==c1 && b<b1 = (Move i D) 
         | c==c1 && b>b1 = (Move i U)  
         | b==b1 && c<c1 = (Move i R)  
         | b==b1 && c>c1 = (Move i L) 
         | y==R = (Move i R)
         | y==L = (Move i L)
         | y==U = (Move i U) 
         | y==D = (Move i D)
getNewMove (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)) (State m ms lvl)
         | distance (b,c) (b1,c1) < 6 && length (getFoodCoords 0 0 m) > 0 = findFood (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) m
         | y==R && getPiece (b,c+1) m == Wall = if b<b1 then (Move i U) else (Move i D) 
         | y==L && getPiece (b,c-1) m == Wall = if b<b1 then (Move i U) else (Move i D) 
         | y==U && getPiece (b-1,c) m == Wall = if c<c1 then (Move i L) else (Move i R) 
         | y==D && getPiece (b+1,c) m == Wall = if c<c1 then (Move i L) else (Move i R) 
         | y==R && getPiece (b,c+1) m == Wall = if b>b1 then (Move i D) else (Move i U) 
         | y==L && getPiece (b,c-1) m == Wall = if b>b1 then (Move i D) else (Move i U) 
         | y==U && getPiece (b-1,c) m == Wall = if c>c1 then (Move i R) else (Move i L) 
         | y==D && getPiece (b+1,c) m == Wall = if c>c1 then (Move i R) else (Move i L) 
         | c==c1 && b<b1 = (Move i U) 
         | c==c1 && b>b1 = (Move i D)  
         | b==b1 && c<c1 = (Move i L)  
         | b==b1 && c>c1 = (Move i R) 
         | y==R = (Move i R)
         | y==L = (Move i L)
         | y==U = (Move i U) 
         | y==D = (Move i D)
@

-}

getNewMove :: Player -> Player -> State -> Play
getNewMove (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Dead)) (State m ms lvl)
         | y==R && getPiece (b,c+1) m == Wall = if b<b1 then (Move i D) else (Move i U) 
         | y==L && getPiece (b,c-1) m == Wall = if b<b1 then (Move i D) else (Move i U) 
         | y==U && getPiece (b-1,c) m == Wall = if c<c1 then (Move i R) else (Move i L) 
         | y==D && getPiece (b+1,c) m == Wall = if c<c1 then (Move i R) else (Move i L) 
         | y==R && getPiece (b,c+1) m == Wall = if b>b1 then (Move i U) else (Move i D) 
         | y==L && getPiece (b,c-1) m == Wall = if b>b1 then (Move i U) else (Move i D) 
         | y==U && getPiece (b-1,c) m == Wall = if c>c1 then (Move i L) else (Move i R) 
         | y==D && getPiece (b+1,c) m == Wall = if c>c1 then (Move i L) else (Move i R) 
         | c==c1 && b<b1 = (Move i D) 
         | c==c1 && b>b1 = (Move i U)  
         | b==b1 && c<c1 = (Move i R)  
         | b==b1 && c>c1 = (Move i L) 
         | y==R = (Move i R)
         | y==L = (Move i L)
         | y==U = (Move i U) 
         | y==D = (Move i D)
getNewMove (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)) (State m ms lvl)
         | distance (b,c) (b1,c1) < 6 && length (getFoodCoords 0 0 m) > 0 = findFood (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) m
         | y==R && getPiece (b,c+1) m == Wall = if b<b1 then (Move i U) else (Move i D) 
         | y==L && getPiece (b,c-1) m == Wall = if b<b1 then (Move i U) else (Move i D) 
         | y==U && getPiece (b-1,c) m == Wall = if c<c1 then (Move i L) else (Move i R) 
         | y==D && getPiece (b+1,c) m == Wall = if c<c1 then (Move i L) else (Move i R) 
         | y==R && getPiece (b,c+1) m == Wall = if b>b1 then (Move i D) else (Move i U) 
         | y==L && getPiece (b,c-1) m == Wall = if b>b1 then (Move i D) else (Move i U) 
         | y==U && getPiece (b-1,c) m == Wall = if c>c1 then (Move i R) else (Move i L) 
         | y==D && getPiece (b+1,c) m == Wall = if c>c1 then (Move i R) else (Move i L) 
         | c==c1 && b<b1 = (Move i U) 
         | c==c1 && b>b1 = (Move i D)  
         | b==b1 && c<c1 = (Move i L)  
         | b==b1 && c>c1 = (Move i R) 
         | y==R = (Move i R)
         | y==L = (Move i L)
         | y==U = (Move i U) 
         | y==D = (Move i D)

{-| Esta função vai ser o elo de ligação da função final com a função 'auxFind' uma vez que esta funçao vai dividir
os argumentos de forma a que a próxima consiga efetuar o que nós queremos.

@
findFood :: Player -> Maze -> Play
findFood pac@(Pacman(PacState (i,(b,c), x, y,z,l) h f j)) m= auxFind pac (getBestFood pac (getFoodCoords 0 0 m)) m
@

-}

findFood :: Player -> Maze -> Play
findFood pac@(Pacman(PacState (i,(b,c), x, y,z,l) h f j)) m= auxFind pac (getBestFood pac (getFoodCoords 0 0 m)) m

{-| Esta função vai receber um pacman, as coordenadas da comida mais próxima e o labirinto e, consequentemente vai devolver
a melhor jogada a tomar para ir comer a comida.

@
auxFind :: Player -> Coords -> Maze -> Play
auxFind (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (x1,y1) m
   | y==R && getPiece (b,c+1) m == Wall = if b<x1 then (Move i D) else (Move i U) 
   | y==L && getPiece (b,c-1) m == Wall = if b<x1 then (Move i D) else (Move i U) 
   | y==U && getPiece (b-1,c) m == Wall = if c<y1 then (Move i R) else (Move i L) 
   | y==D && getPiece (b+1,c) m == Wall = if c<y1 then (Move i R) else (Move i L) 
   | y==R && getPiece (b,c+1) m == Wall = if b>x1 then (Move i U) else (Move i D) 
   | y==L && getPiece (b,c-1) m == Wall = if b>x1 then (Move i U) else (Move i D) 
   | y==U && getPiece (b-1,c) m == Wall = if c>y1 then (Move i L) else (Move i R) 
   | y==D && getPiece (b+1,c) m == Wall = if c>y1 then (Move i L) else (Move i R) 
   | c==y1 && b<x1 = (Move i D) 
   | c==y1 && b>x1 = (Move i U)  
   | b==x1 && c<y1 = (Move i R)  
   | b==x1 && c>y1 = (Move i L) 
   | y==R = (Move i R)
   | y==L = (Move i L)
   | y==U = (Move i U) 
   | y==D = (Move i D)
   | y==Null = (Move i D) 
@
-}

auxFind :: Player -> Coords -> Maze -> Play
auxFind (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (x1,y1) m
   | y==R && getPiece (b,c+1) m == Wall = if b<x1 then (Move i D) else (Move i U) 
   | y==L && getPiece (b,c-1) m == Wall = if b<x1 then (Move i D) else (Move i U) 
   | y==U && getPiece (b-1,c) m == Wall = if c<y1 then (Move i R) else (Move i L) 
   | y==D && getPiece (b+1,c) m == Wall = if c<y1 then (Move i R) else (Move i L) 
   | y==R && getPiece (b,c+1) m == Wall = if b>x1 then (Move i U) else (Move i D) 
   | y==L && getPiece (b,c-1) m == Wall = if b>x1 then (Move i U) else (Move i D) 
   | y==U && getPiece (b-1,c) m == Wall = if c>y1 then (Move i L) else (Move i R) 
   | y==D && getPiece (b+1,c) m == Wall = if c>y1 then (Move i L) else (Move i R) 
   | c==y1 && b<x1 = (Move i D) 
   | c==y1 && b>x1 = (Move i U)  
   | b==x1 && c<y1 = (Move i R)  
   | b==x1 && c>y1 = (Move i L) 
   | y==R = (Move i R)
   | y==L = (Move i L)
   | y==U = (Move i U) 
   | y==D = (Move i D)
   | y==Null = (Move i D) 

{-| Esta função vai receber dois contadores (inicialmente são colocados a 0) e um labirinto e consequentemente devolve 
as coordenadas de todas as comidas grandes.

@
getFoodCoords :: Int -> Int -> Maze -> [Coords]
getFoodCoords n y [] = []
getFoodCoords n y (h:t) = auxFoodCorridor n y h ++ getFoodCoords (n+1) y t

auxFoodCorridor :: Int -> Int -> Corridor -> [Coords]
auxFoodCorridor n y [] = []
auxFoodCorridor n y (h:t)
  | h==Food Big = (n,y): auxFoodCorridor n (y+1) t
  | otherwise= auxFoodCorridor n (y+1) t
@
-}

--coordenadas de todas as comidas grandes
getFoodCoords :: Int -> Int -> Maze -> [Coords]
getFoodCoords n y [] = []
getFoodCoords n y (h:t) = auxFoodCorridor n y h ++ getFoodCoords (n+1) y t

--coordenadas de um corredor
auxFoodCorridor :: Int -> Int -> Corridor -> [Coords]
auxFoodCorridor n y [] = []
auxFoodCorridor n y (h:t)
  | h==Food Big = (n,y): auxFoodCorridor n (y+1) t
  | otherwise= auxFoodCorridor n (y+1) t

{-| Esta função vai receber o nosso jogador do tipo pacman e a lista das coordenadas das comidas grandes e, depois, vai
verificar qual é a comida que se encontra mais perto do nosso jogador.

@
getBestFood :: Player -> [Coords] -> Coords
getBestFood (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (h1:h2:t)
  | distance (b,c) h1 < distance (b,c) h2 = h1
  | otherwise= getBestFood (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (h2:t)
@

-}

--comida mais proxima
getBestFood :: Player -> [Coords] -> Coords
getBestFood (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (h1:h2:t)
  | distance (b,c) h1 < distance (b,c) h2 = h1
  | otherwise= getBestFood (Pacman(PacState (i,(b,c), x, y,z,l) h f j)) (h2:t)

{-| Por fim, esta função vai ser a função que dado um ID e um estado vai devolver a melhor jogada a tomar tendo em conta
todas as circunstâncias.

@
bot :: Int -> State -> Maybe Play
bot x s@(State m ms l) = Just (getNewMove (whereIsPacman ms) (findGhost (getID (getMinimumDistance (getAllDistances (coordsPacman ms) (coordsGhosts ms)))) (whereAreGhosts ms)) (State (updateMaze (whereIsPacman ms) m) ms l))
@
-}

bot :: Int -> State -> Maybe Play
bot x s@(State m ms l) = Just (getNewMove (whereIsPacman ms) (findGhost (getID (getMinimumDistance (getAllDistances (coordsPacman ms) (coordsGhosts ms)))) (whereAreGhosts ms)) (State (updateMaze (whereIsPacman ms) m) ms l))
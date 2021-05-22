{-|
Module : Tarefa1
Description : Módulo Haskell com funções necessárias para a criação de um labirinto do pacman.
Copyright : Telmo Maciel <a96569@alunos.uminho.pt>;
            Tiago Soares <a97381@alunos.uminho.pt>

= Introdução

 Na tarefa 1 exploramos a ideia de criar labirintos. Foi uma tarefa importante para começar
a projetar todo o resto do jogo.

= Objetivos

 O nosso objetivo era criar labirintos aleatórios dadas as medidas dos mesmos. Seguimos um raciocínio
lógico e fomos desenvolvendo tudo a partir daí. Começando com conjuntos (paredes, corredores...) e fomos
projetando o que se tornaria o labirinto. Na nossa forma de pensar, queríamos fazer com que a casa dos
fantasmas (construída à parte) encaixasse no labirinto. Para isso, recorremos a uma função que identificava
o centro do labirinto e construímos a partir daí. Fomos adicionando tudo o resto sequencialmente e por
necessidade, como espaços em branco no sítios do túneis ou as paredes em volta do labirinto.

= Discussão e Conclusão

 Cremos que fizemos um bom trabalho nesta tarefa, utilizamos reciocínios originais e fomos adaptando tudo
de modo a chegar ao resultado pretendido.

-}
module Tarefa1 where

import Types
import Generator 



{-| A função 'generateWall' cria um corredor feito só de paredes.


      A função é definida da seguinte forma:

@
generateWall :: Int -> Corridor
generateWall 0 = []
generateWall x = Wall : generateWall (x-1)
@


  == Exemplos de utilização:

  >>> generateWall 5
  [#,#,#,#,#]

  >>> generateWall 10
  [#,#,#,#,#,#,#,#,#,#]

-}
generateWall :: Int -> Corridor
generateWall 0 = []
generateWall x = Wall : generateWall (x-1)


{-| A funçao generateMaze''  gera um labirinto totalmente aleatório, com paredes em cima e em baixo do mesmo.

Esta função necessita de uma segunda, que faz com que seja possĩvel unir um corredor só de paredes ao resto do labirinto gerado pela própria função.


A função generateMaze'' é definida da seguinte forma:

@
generateMaze'' :: Int -> Int -> Int -> Maze
generateMaze'' l a s = attachWallToMaze ( (generateWall l) : (geraLabirinto l (a-2) s)) (generateWall l) 
@

-}

generateMaze'' :: Int -> Int -> Int -> Maze
generateMaze'' l a s = attachWallToMaze ( (generateWall l) : (geraLabirinto l (a-2) s)) (generateWall l) 

{-| Esta função, como dito anteriormente, faz com que seja possível a união dos vários corredores.

@
attachWallToMaze :: Maze-> Corridor -> Maze
attachWallToMaze [] c = [c]
attachWallToMaze (x:xs) c = x : attachWallToMaze xs c
@

-}
attachWallToMaze :: Maze-> Corridor -> Maze
attachWallToMaze [] c = [c]
attachWallToMaze (x:xs) c = x : attachWallToMaze xs c

{-| A seguinte função recebe um número inteiro (futuramente a largura do labirinto) e dá como resultado a casa dos fantasmas:

@
ghostHouse :: Int -> Maze
ghostHouse i 
  | even i = [ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],  (No caso de ser dado um número par)
               [Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty], 
               [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
               [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty], 
               [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]  
  | otherwise = [ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],  (No caso de ser dado um número ímpar)
                  [Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],
                  [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
                  [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ] 
@


  == Exemplos de utilização:

  >>> ghostHouse 24
  [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],  
   [Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty], 
   [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
   [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty], 
   [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]  

  >>> ghostHouse 35
  [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty], 
   [Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],
   [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
   [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
   [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ] 

  
-}

ghostHouse :: Int -> Maze
ghostHouse i 
  | even i = [ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
               [Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty], 
               [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
               [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty], 
               [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]  
  | otherwise = [ [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                  [Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],
                  [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
                  [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
                  [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] ] 

{-| Para começar o processo de colocar a casa dos fantasmas no centro do labirinto gerado, optamos por obter os 5 corredores centrais do labirinto. 
Para tal, criamos uma função para esse propósito.
     
@
get5MiddleCorridor :: Maze -> [Corridor]
get5MiddleCorridor generateMaze''
    | even m = [b!!(d-3),b!!(d-2),b!!(d-1),b!!d,b!!(d+1)]
    | otherwise = [b!!(d-2),b!!(d-1),b!!(d),b!!(d+1),b!!(d+2)]
        where m = length generateMaze''        
              b = generateMaze''
              d = (div m 2)
@


-}

get5MiddleCorridor :: Maze -> [Corridor]
get5MiddleCorridor generateMaze''
    | even m = [b!!(d-3),b!!(d-2),b!!(d-1),b!!d,b!!(d+1)]
    | otherwise = [b!!(d-2),b!!(d-1),b!!(d),b!!(d+1),b!!(d+2)]
        where m = length generateMaze''        
              b = generateMaze''
              d = (div m 2)

{-| Esta função recebe dois labirintos e faz as substituições nos seus corredores.

@
replaceCorridor :: Maze -> Maze -> Maze
replaceCorridor _ [] = []
replaceCorridor [] _ = []
replaceCorridor (l:ls) (y:ys) = replaceCorridorAux e l y : replaceCorridor ls ys
  where e= (div (length y) 2)-5
@

-}

replaceCorridor :: Maze -> Maze -> Maze
replaceCorridor _ [] = []
replaceCorridor [] _ = []
replaceCorridor (l:ls) (y:ys) = replaceCorridorAux e l y : replaceCorridor ls ys
  where e= (div (length y) 2)-5

{-| A próxima função é utilizada como auxiliar da anterior, mas só utiliza corredores.

Esta função recebe dois corredores e, dado um índice, substitui um corredor no centro de outro.

@
replaceCorridorAux :: Int -> Corridor -> Corridor -> Corridor
replaceCorridorAux 0 (l:ls) x = l : replaceCorridorAux 0 ls (tail x)
replaceCorridorAux _ [] l = l
replaceCorridorAux n (x:xs) (y:ys) = y : replaceCorridorAux (n-1) (x:xs) ys
@

-}

replaceCorridorAux :: Int -> Corridor -> Corridor -> Corridor
replaceCorridorAux 0 (l:ls) x = l : replaceCorridorAux 0 ls (tail x)
replaceCorridorAux _ [] l = l
replaceCorridorAux n (x:xs) (y:ys) = y : replaceCorridorAux (n-1) (x:xs) ys


{-| A próxima função recebe como primeiro argumento o labirinto e como segundo os 5 corredores já substituidos, devolvendo o labirinto completo.

@
insertGhosHouse :: Maze -> Maze -> Maze
insertGhosHouse generateMaze'' replaceCorridor 
  | even b = take (c-3) (generateMaze'') ++ replaceCorridor ++ drop (c+2) (generateMaze'')
  | otherwise = take (c-2) (generateMaze'') ++ replaceCorridor ++ drop (c+3) (generateMaze'')
     where b= length generateMaze'' 
           c = (div b 2)
@


-}

insertGhosHouse :: Maze -> Maze -> Maze
insertGhosHouse generateMaze'' replaceCorridor 
  | even b = take (c-3) (generateMaze'') ++ replaceCorridor ++ drop (c+2) (generateMaze'')
  | otherwise = take (c-2) (generateMaze'') ++ replaceCorridor ++ drop (c+3) (generateMaze'')
     where b= length generateMaze'' 
           c = (div b 2)

{-| A função 'mazeWithGhostHouse' recebe 3 números inteiros, nomeadamente uma largura, altura e seed e devolve um labirinto com a casa dos fantasmas já inserida.

@
mazeWithGhostHouse :: Int -> Int -> Int -> Maze
mazeWithGhostHouse l a s = insertGhosHouse (generateMaze'' l a s) (replaceCorridor (ghostHouse l) (get5MiddleCorridor (generateMaze'' l a s)))
    where y=(div (length (head (generateMaze'' l a s))) 2)-5
@

-}

mazeWithGhostHouse :: Int -> Int -> Int -> Maze
mazeWithGhostHouse l a s = insertGhosHouse (generateMaze'' l a s) (replaceCorridor (ghostHouse l) (get5MiddleCorridor $ generateMaze'' l a s))
    where y=(div (length (head (generateMaze'' l a s))) 2)-5

{-| De seguida, o objetivo era fazer com que o labirinto ficasse cercado por paredes, e, para isso, foi criada a função 'placeSideWalls' que, dado um labirinto já com paredes no topo e na base, faz as substituições nas laterais.

@
placeSideWalls :: Maze -> [Corridor]  
placeSideWalls [] = []
placeSideWalls mazeWithGhostHouse = ([Wall] ++ (init (tail (head mazeWithGhostHouse))) ++ [Wall]) : placeSideWalls (tail mazeWithGhostHouse)
@


-}

placeSideWalls :: Maze -> [Corridor]  
placeSideWalls [] = []
placeSideWalls mazeWithGhostHouse = ([Wall] ++ (init (tail (head mazeWithGhostHouse))) ++ [Wall]) : placeSideWalls (tail mazeWithGhostHouse)

{-| O último passo é abrir um espaço de cada lado que, futuramente, funciona como túnel para o pacman.

@
mazeTunel :: Maze -> Maze
mazeTunel placeSideWalls
    | odd m = take (d) (placeSideWalls) ++ [[Empty] ++ (init (tail (placeSideWalls!!(d)))) ++ [Empty]] ++ drop (d+1) (placeSideWalls)
    | otherwise = take (d-1) (placeSideWalls) ++ [[Empty] ++ (init (tail (placeSideWalls!!(d-1)))) ++ [Empty]] ++ [[Empty] ++ (init (tail (placeSideWalls!!(d)))) ++ [Empty]] ++ drop (d+1) (placeSideWalls)
       where m= length placeSideWalls
             d= (div m 2)
@

-}

mazeTunel :: Maze -> Maze
mazeTunel placeSideWalls
    | odd m = take (d) (placeSideWalls) ++ [[Empty] ++ (init (tail (placeSideWalls!!(d)))) ++ [Empty]] ++ drop (d+1) (placeSideWalls)
    | otherwise = take (d-1) (placeSideWalls) ++ [[Empty] ++ (init (tail (placeSideWalls!!(d-1)))) ++ [Empty]] ++ [[Empty] ++ (init (tail (placeSideWalls!!(d)))) ++ [Empty]] ++ drop (d+1) (placeSideWalls)
       where m= length placeSideWalls
             d= (div m 2)

{-| Por fim, resta definir a função final que, dados 3 números inteiros (largura, altura e seed), devolve o labirinto já com todos os requisitos.

@
generateMaze :: Int -> Int -> Int -> Maze
generateMaze l a s = mazeTunel (placeSideWalls (mazeWithGhostHouse l a s))
@


  == Exemplo de utilização:

  >>> generateMaze 15 10 9
  [[#,#,#,#,#,#,#,#,#,#,#,#,#,#,#],[#,.,#,#,.,.,.,.,.,.,.,#,.,.,#],[#,#, , , , , , , , , , , ,.,#],[#,., ,#,#,#, , , ,#,#,#, ,.,#],[ ,., ,#, , , , , , , ,#, ,., ],[ ,#, ,#,#,#,#,#,#,#,#,#, ,#, ],[#,#, , , , , , , , , , , ,.,#],[#,.,.,.,.,.,#,.,.,#,.,.,.,#,#],[#,.,.,.,.,.,#,.,#,#,#,.,#,#,#],[#,#,#,#,#,#,#,#,#,#,#,#,#,#,#]]



-}

generateMaze :: Int -> Int -> Int -> Maze
generateMaze l a s = mazeTunel $ placeSideWalls $ mazeWithGhostHouse l a s

{-| A próxima função faz quase o mesmo que a anterior, apenas com uma pequena diferença. Neste caso, o labirinto é dado já com a sua forma final.

(Mesmo não tendo sido proposta a criação desta função no enunciado, decidimos fazê-la para os testes terem um aspeto mais agradável e facilitar o raciocínio.)

@
printMazeFinal :: Int -> Int -> Int -> IO ()
printMazeFinal l a s = imprimeLabirinto (generateMaze l a s)
@


  == Exemplos de utilização:

  >>> generateWall 15 10 9 (Usando o exemplo anterior)
  ###############
  #.##.......#..#
  ##           .#
  #. ###   ### .#
   . #       # . 
   # ######### # 
  ##           .#
  #.....#..#...##
  #.....#.###.###
  ###############


-}

printMazeFinal :: Int -> Int -> Int -> IO ()
printMazeFinal l a s = imprimeLabirinto $ generateMaze l a s


{- |
Module : Tarefa5
Description : Módulo Haskell com funções necessárias para que os fantasmas joguem automaticamente.
Copyright : Telmo Maciel <a96569@alunos.uminho.pt>;
            Tiago Soares <a97381@alunos.uminho.pt>

= Introdução

  A tarefa 5 é a tarefa onde fazemos com que os fantasmas persigam o pacman caso estejam vivos. Caso contrário,
faz com que os fantasmas fujam.

= Objetivos

  Nesta tarefa o objetivo era implementar um movimento para os fantasmas, e, antes de tudo, tinhamos de alterar
a função play da tarefa 2, coisa que já modificamos ao mesmo tempo que fizemos as alterações da tarefa 4.

  O nosso principal objetivo foi fazer com que os fantasmas assumissem um comportamento inteligente, perseguindo 
o pacman, ou fugindo no caso deste estar em modo Mega.

  Primeiro de tudo separamos as ideias em dois casos, o caso do pacman estar em modo Mega e o caso que ele esteja
em modo Normal. Para o segundo caso fizemos uma coisa mais simples, isto é, primeiro de tudo, quando o pacman 
passasse numa casa com comida grande (quando se transforma em Mega) o nosso ghost vai mudar a orientação dele em 
180 graus, como por exemplo, se ele estivesse a andar para a direita no momento imediatamente antes do pacman comer
a comida grande, o nosso ghost passaria a deslocar-se para a esquerda e a mesma coisa para as outras orientações e,
depois disto, sempre que ele encontrasse uma parede passaria a deslocar-se para a sua direita até acontecer o mesmo
novamente.

  Agora para o caso do pacman estar em modo Normal o nosso pensamento foi completamente diferente. Neste caso, o 
que nós fizemos foi analisar as duas coordenadas (as do pacman e as do fantasma) e, com isto, obter o melhor movimento
possível a realizar, por exemplo, se o nosso ghost se encontrasse imediatamente abaixo do pacman, a nossa funçao ia 
analisar as coordenadas e determinar se o nosso ghost se iria mover para cima ou para baixo e assim para todas as outras
situações.

= Discussão e Conclusão

  Apesar de esta tarefa ter parecido fácil inicialmente, ao longo desta tivemos vários problemas a resolver visto 
que havia várias alternativas a tomar tendo em contas as coordenadas dos dois players. Apesar de surgirem vários 
erros nas funções ao longo do desenvolvimento das mesmas fomos conseguindo corrigir e terminar a tarefa.

  Em conclusão, penso que a função 'scatterMode' (função que faz o ghost fugir do pacman) tenha ficado sem qualquer 
tipo de erro, visto que a função era muito mais simples que a função 'chaseMode' (função que vai atrás dos pacmans), 
isto porque a segunda vai ter muitos mais casos a analisar.

  Em relação à segunda função, fazendo os testes por nós a função pareceu estar bem construída e eficaz, apesar de
ter levado tempo até chegar ao final da mesma (devido aos erros anteriormente referidos). 

  Por fim, resta-nos salientar que esta foi a melhor forma que conseguimos realizar a tarefa e, apesar de termos 
testado de várias formas diferentes, temos noção que possam ter ficado casos por cobrir, coisa que mais tarde, depois
de surgirem os erros, se pode corrigir sem probelmas.
  
-}
module Tarefa5 where 

import Types
import Tarefa1
import Tarefa2

{-| Esta função vai receber um estado e o ID do jogador e vai devolver a jogada que esse jogador vai fazer para fugir do pacman. 

@
scatterMode :: State -> Int -> Play
scatterMode s@(State m ms l) id 
   | nextPiece ori player m == Wall = auxScatter id ori 
   | otherwise= (Move id ori) 
       where  ori =getPlayerOrientation (getPlayer id ms)
              player = (getPlayer id ms)
@
-}

scatterMode :: State -> Int -> Play
scatterMode s@(State m ms l) id 
   | nextPiece ori player m == Wall = auxScatter id ori 
   | otherwise= (Move id ori) 
       where  ori =getPlayerOrientation (getPlayer id ms)
              player = (getPlayer id ms)

{-| Esta função é apenas uma auxiliar da 'scatterMode'.

@
auxScatter :: Int -> Orientation -> Play
auxScatter id ori 
   | ori==R = (Move id D)
   | ori==L = (Move id U)
   | ori==D = (Move id L)
   | ori==U = (Move id R)
@
-}

auxScatter :: Int -> Orientation -> Play
auxScatter id ori 
   | ori==R = (Move id D)
   | ori==L = (Move id U)
   | ori==D = (Move id L)
   | ori==U = (Move id R)

{-| Esta função vai receber o estado do jogo e o ID do ghost e vai devolver a jogada que esse fantasma tem que fazer para apanhar o pacman.

@
chaseMode :: State -> Int -> Play
chaseMode s@(State m ms l) id = isGhostAlive pac (head lista) s
  where pac= whereIsPacman ms
        lista= whereAreGhosts ms
@
-}

chaseMode :: State -> Int -> Play
chaseMode s@(State m ms l) id = isGhostAlive pac (head lista) s
  where pac= whereIsPacman ms
        lista= whereAreGhosts ms

{-| Esta função analisa as coordenadas do pacman e do ghost e devolve o movimento que o fantasma precisa de executar para apanhar o pacman.

@
isGhostAlive :: Player -> Player -> State -> Play
isGhostAlive (Pacman (PacState (i,(b,c), x, y,z,l) h f Dying)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)) s@(State m ms lvl)= (Move i1 Null)
isGhostAlive (Pacman (PacState (i,(b,c), x, y,z,l) h f mode)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)) s@(State m ms lvl)
   | (b1,c1)==((div(length (m)) 2),(div (length (head m)) 2))= (Move i1 U)
   | (b1,c1)==((div(length (m)) 2)-1,(div (length (head m)) 2))= (Move i1 U)
   | b1==b && c1==c = (Move i1 Null)
   | y1==R && getPiece (b1,c1+1) m == Wall = if b<b1 then (Move i1 U) else (Move i1 D) 
   | y1==L && getPiece (b1,c1-1) m == Wall = if b<b1 then (Move i1 U) else (Move i1 D) 
   | y1==U && getPiece (b1-1,c1) m == Wall = if c<c1 then (Move i1 L) else (Move i1 R) 
   | y1==D && getPiece (b1+1,c1) m == Wall = if c<c1 then (Move i1 L) else (Move i1 R) 
   | y1==R && getPiece (b1,c1+1) m == Wall = if b>b1 then (Move i1 D) else (Move i1 U) 
   | y1==L && getPiece (b1,c1-1) m == Wall = if b>b1 then (Move i1 D) else (Move i1 U) 
   | y1==U && getPiece (b1-1,c1) m == Wall = if c>c1 then (Move i1 R) else (Move i1 L) 
   | y1==D && getPiece (b1+1,c1) m == Wall = if c>c1 then (Move i1 R) else (Move i1 L) 
   | c==c1 && b<b1 = (Move i1 U) 
   | c==c1 && b>b1 = (Move i1 D)  
   | b==b1 && c<c1 = (Move i1 L)  
   | b==b1 && c>c1 = (Move i1 R) 
   | y1==R = (Move i1 R)
   | y1==L = (Move i1 L)
   | y1==U = (Move i1 U) 
   | y1==D = (Move i1 D)
   | y1==Null = (Move i1 D) 
@
-}

isGhostAlive :: Player -> Player -> State -> Play
isGhostAlive (Pacman (PacState (i,(b,c), x, y,z,l) h f Dying)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)) s@(State m ms lvl)= (Move i1 Null)
isGhostAlive (Pacman (PacState (i,(b,c), x, y,z,l) h f mode)) (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)) s@(State m ms lvl)
   | (b1,c1)==((div(length (m)) 2),(div (length (head m)) 2))= (Move i1 U)
   | (b1,c1)==((div(length (m)) 2)-1,(div (length (head m)) 2))= (Move i1 U)
   | b1==b && c1==c = (Move i1 Null)
   | y1==R && getPiece (b1,c1+1) m == Wall = if b<b1 then (Move i1 U) else (Move i1 D) 
   | y1==L && getPiece (b1,c1-1) m == Wall = if b<b1 then (Move i1 U) else (Move i1 D) 
   | y1==U && getPiece (b1-1,c1) m == Wall = if c<c1 then (Move i1 L) else (Move i1 R) 
   | y1==D && getPiece (b1+1,c1) m == Wall = if c<c1 then (Move i1 L) else (Move i1 R) 
   | y1==R && getPiece (b1,c1+1) m == Wall = if b>b1 then (Move i1 D) else (Move i1 U) 
   | y1==L && getPiece (b1,c1-1) m == Wall = if b>b1 then (Move i1 D) else (Move i1 U) 
   | y1==U && getPiece (b1-1,c1) m == Wall = if c>c1 then (Move i1 R) else (Move i1 L) 
   | y1==D && getPiece (b1+1,c1) m == Wall = if c>c1 then (Move i1 R) else (Move i1 L) 
   | c==c1 && b<b1 = (Move i1 U) 
   | c==c1 && b>b1 = (Move i1 D)  
   | b==b1 && c<c1 = (Move i1 L)  
   | b==b1 && c>c1 = (Move i1 R) 
   | y1==R = (Move i1 R)
   | y1==L = (Move i1 L)
   | y1==U = (Move i1 U) 
   | y1==D = (Move i1 D)
   | y1==Null = (Move i1 D) 

{-| Por fim, esta vai receber um estado e devolve automaticamente todos os movimentos que os fantasmas vão fazer, quer seja para apanhar o pacman quer seja para fugir do mesmo.

@
ghostPlay :: State -> [Play]
ghostPlay s@(State m [x] l)= if isPacman x then [] else []
ghostPlay s@(State m ms l) 
   | getGhostMode (head lista)== Dead  = scatterMode s (getPlayerID (head lista)) : ghostPlay remove
   | getGhostMode (head lista)== Alive = chaseMode s x : ghostPlay remove
      where lista= whereAreGhosts ms
            pac = whereIsPacman ms
            remove = removePlayerFromState (head lista) s
            x= getPlayerID (head lista)
@
-}

ghostPlay :: State -> [Play]
ghostPlay s@(State m [x] l)= if isPacman x then [] else []
ghostPlay s@(State m ms l) 
   | getGhostMode (head lista)== Dead  = scatterMode s (getPlayerID (head lista)) : ghostPlay remove
   | getGhostMode (head lista)== Alive = chaseMode s x : ghostPlay remove
      where lista= whereAreGhosts ms
            pac = whereIsPacman ms
            remove = removePlayerFromState (head lista) s
            x= getPlayerID (head lista)
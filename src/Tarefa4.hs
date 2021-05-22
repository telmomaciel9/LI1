{-|
Module : Tarefa4
Description : Módulo Haskell com funções necessárias para fazer com que todos os jogadores obtenham uma jogada.
Copyright : Telmo Maciel <a96569@alunos.uminho.pt>;
            Tiago Soares <a97381@alunos.uminho.pt>

= Introdução

  A tarefa 4 é a tarefa fundamental para que o jogo ganhe "vida". É nesta tarefa que implementamos funções para 
que todos os jogadores realizem um movimento, fazendo com que o jogo ganhe alguma dinâmica.

= Objetivos

  O objetivo desta tarefa foi calcular o efeito da passagem de um instante de tempo num estado do jogo.
Para tal, primeiro tivemos de efetuar algumas alterações na tarefa 2 de modo a que fosse possível efetuar 
jogadas com os fantasmas. A prioridade foi a possibilidade de movimentar os fantasmas mas também foram 
implementadas, na mesma tarefa, as seguintes alterações:

* Fazer com que o pacman abrisse e fechasse a boca alternadamente entre cada jogada;
* Fazer com que o pacman perdesse tempo em cada jogada, no caso de ele estar em modo Mega;
* Fazer com que o pacman voltasse ao modo Normal caso o tempo mega dele voltasse a 0;
* Fazer com que os fantasmas voltassem a modo Alive caso não exista nenhum pacman em modo Mega.

  E, após estas alterações todas, tentamos implementar na tarefa 4 as seguintes condições:

  Os jogadores deviam progredir n jogadas em cada iteração da função, por exemplo:

* Se um jogador tivesse velocidade 0.5 este devia progredir de 2 em 2 iterações;
* Se um jogador tivesse velocidade 1.5 este devia progredir de 3 a cada 2 iterações;
* Se um jogador tivesse velocidade 2.0 este devia progredir de 2 em cada iteração.

  Por fim, tentamos criar uma função que realizasse todos os movimentos que os jogadores faziam em cada iteração.

= Discussão e Conclusão

  Em conclusão, esta tarefa foi bastante rápida de realizar, exceto as alterações da tarefa 2, visto que tivemos de 
analisar o código todo e pensar onde poderíamos implementar todas as alterações necessárias. Pensamos que esta tarefa 
ficou da melhor forma possível e o mais compacta possível e que, apesar de não termos um local para realizar testes 
como tivemos na primeira fase, que ficou bem feita e aceita todos os casos possíveis.

-}
module Tarefa4 where 

import Types
import UI.NCurses
import Tarefa2
import Tarefa1 
import Tarefa5
import Tarefa6

-- | Tempo predefinido de 250ms
defaultDelayTime = 250

{-| Esta função tem como objetivo rotacionar o pacman conforme se prime as teclas.

@
rotatePlayer (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) u 
  | u==KeyUpArrow  = (Pacman (PacState (i,(b,c), x, U,z,l) h f j))
  | u==KeyDownArrow  = (Pacman (PacState (i,(b,c), x, D,z,l) h f j))
  | u==KeyLeftArrow  = (Pacman (PacState (i,(b,c), x, L,z,l) h f j))
  | u==KeyRightArrow  = (Pacman (PacState (i,(b,c), x, R,z,l) h f j))
@


-}

rotatePlayer :: Player -> Key -> Player
rotatePlayer (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) u 
  | u==KeyUpArrow  = (Pacman (PacState (i,(b,c), x, U,z,l) h f j))
  | u==KeyDownArrow  = (Pacman (PacState (i,(b,c), x, D,z,l) h f j))
  | u==KeyLeftArrow  = (Pacman (PacState (i,(b,c), x, L,z,l) h f j))
  | u==KeyRightArrow  = (Pacman (PacState (i,(b,c), x, R,z,l) h f j))

{-| Esta função tem como objetivo fazer com que todas as entidades (Pacman e Ghosts) joguem.

@
playAll :: Int -> [Player] -> State -> State 
playAll n [] s = s
playAll n ((Pacman (PacState (i,(b,c), x, y,z,l) h f Dying)):xs) s = s
playAll n ((Pacman (PacState (i,(b,c), x, y,z,l) h f mode)):xs) s = playAll n xs (play (Move i y) s)
playAll n  ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)):xs) s = playAll n xs (play (auxgetMoves i1 (ghostPlay s)) s) 
playAll n  ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Dead)):xs) s 
  | mod n 2 ==0 = playAll n xs (play (scatterMode s i1) s)
  | otherwise = playAll n xs s
@


-}

playAll :: Int -> [Player] -> State -> State 
playAll n [] s = s
playAll n ((Pacman (PacState (i,(b,c), x, y,z,l) h f Dying)):xs) s = s
--playAll n ((Pacman (PacState (i,(b,c), x, y,z,l) h f mode)):xs) s = playAll n xs (play (bot i s) s) -- Função para testar o bot
playAll n ((Pacman (PacState (i,(b,c), x, y,z,l) h f mode)):xs) s = playAll n xs (play (Move i y) s)
playAll n  ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Alive)):xs) s = playAll n xs (play (auxgetMoves i1 (ghostPlay s)) s) 
playAll n  ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) Dead)):xs) s 
  | mod n 2 ==0 = playAll n xs (play (scatterMode s i1) s)
  | otherwise = playAll n xs s 

{-| Função auxiliar da 'playAll' (no caso dos Ghosts) para obter o movimento do respetivo fantasma.

@
auxgetMoves :: Int -> [Play] -> Play
auxgetMoves n [] = (Move n Null)
auxgetMoves n l = (getIDPlays l n) 
@


-}

auxgetMoves :: Int -> [Play] -> Play
auxgetMoves n [] = (Move n Null)
auxgetMoves n l = (getIDPlays l n) 

{-| Função que complementa a 'auxgetMoves' para determinar a 'Play' consoante o ID do fantasma desejado.

@
getIDPlays :: [Play] -> Int -> Play
getIDPlays ((Move x ori):t) id
  | x==id = (Move x ori)
  | otherwise= getIDPlays t id
@


-}

getIDPlays :: [Play] -> Int -> Play
getIDPlays ((Move x ori):t) id
  | x==id = (Move x ori)
  | otherwise= getIDPlays t id

{-| Esta função reduz o tempo do jogador do tipo Pacman caso ele esteja em modo Mega.

@
reduceTimeMega :: [Player] -> [Player]
reduceTimeMega [] = []
reduceTimeMega ((Pacman (PacState (i,(b,c), x, y,z,l) h f mode)):xs)
  | mode== Mega && h>0.25 = ((Pacman (PacState (i,(b,c), x, y,z,l) (h-0.25) f Mega)):xs)
  | mode== Mega = ((Pacman (PacState (i,(b,c), x, y,z,l) 0 f Normal)): changeSpeedGhost xs)
  | otherwise= ((Pacman (PacState (i,(b,c), x, y,z,l) 0 f mode)):xs)
@


-}

reduceTimeMega :: [Player] -> [Player]
reduceTimeMega [] = []
reduceTimeMega ((Pacman (PacState (i,(b,c), x, y,z,l) h f mode)):xs)
  | mode== Mega && h>0.25 = ((Pacman (PacState (i,(b,c), x, y,z,l) (h-0.25) f Mega)):xs)
  | mode== Mega = ((Pacman (PacState (i,(b,c), x, y,z,l) 0 f Normal)): changeSpeedGhost xs)
  | otherwise= ((Pacman (PacState (i,(b,c), x, y,z,l) 0 f mode)):xs)

{-| Esta função muda a velocidade do jogador do tipo Ghost, ficando mais lento em modo "Dead".

@
changeSpeedGhost :: [Player] -> [Player]
changeSpeedGhost [] = []
changeSpeedGhost ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):xs)
  | d== Dead = (Ghost(GhoState (i1,(b1,c1), (x1*2.0), y1,z1,l1) Alive)): changeSpeedGhost xs
  | otherwise = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)): changeSpeedGhost xs
@


-}

changeSpeedGhost :: [Player] -> [Player]
changeSpeedGhost [] = []
changeSpeedGhost ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):xs)
  | d== Dead = (Ghost(GhoState (i1,(b1,c1), (x1*2.0), y1,z1,l1) Alive)): changeSpeedGhost xs
  | otherwise = (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)): changeSpeedGhost xs

{-| Esta função tem como objetivo fazer com que todos os jogadores obtenham um movimento.

@
passTime :: Int  -> State -> State
passTime x s@(State m ms l) = playAll x (reduceTimeMega (ordena ms)) s
@


-}

passTime :: Int  -> State -> State
passTime x s@(State m ms l) = playAll x (reduceTimeMega (ordena ms)) s
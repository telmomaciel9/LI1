-- | Este módulo define os tipos de dados comuns a várias tarefas.
module Types where

import Data.List

-- * Tipos de dados auxiliares.

-- | As várias ações dos jogadores.
data PlayAction 
   = EatLittle      -- ^ Come comida pequena.
   | EatBig         -- ^ Come comida grande.
   | EatGhost       -- ^ Come um fantasma.
   | Die            -- ^ Morre.
   | MoveFront      -- ^ Move na mesma direção.
   | Collide        -- ^ Bate contra uma parede

-- | Um estado do jogo.
data State = State 
    {
        maze :: Maze                 -- ^ Labirinto.
    ,   playersState :: [Player]     -- ^ Lista de jogadores.
    ,   level :: Int                 -- ^ Nível do jogo.
    }

-- | Labirinto é um conjunto de corredores.
type Maze = [Corridor]

-- | Corredor é um conjunto de peças.
type Corridor = [Piece]

-- | As várias peças de um jogo.
data Piece 
   =  Food FoodType     -- ^ Peça do tipo comida.
   | PacPlayer Player   -- ^ Peça do tipo jogador.
   | Empty              -- ^ Peça do tipo vazio.
   | Wall               -- ^ Peça do tipo parede.
   deriving (Eq)

-- | Os dois tipos de jogador.
data Player 
  =  Pacman PacState  -- ^ Jogador do tipo pacman.
  | Ghost GhoState    -- ^ Jogador do tipo fantasma.
  deriving (Eq)

-- | As várias orientações.
data Orientation 
  = L         -- ^ Esquerda.
  | R         -- ^ Direita.
  | U         -- ^ Para cima.
  | D         -- ^ Para baixo.
  | Null      -- ^ Fica no sítio.
  deriving (Eq,Show)

-- | Um jogador do tipo pacman.
data PacState= PacState 
    {   
        pacState :: PlayerState      -- ^ Estado do pacman.
    ,   timeMega :: Double           -- ^ Tempo que resta na posição Mega.
    ,   openClosed :: Mouth          -- ^ Maneira que se encontra a boca.
    ,   pacmanMode :: PacMode        -- ^ Como se encontra o pacman (Normal, Mega ou Dying).
    
    } deriving Eq

-- | Um manager do jogo.
data Manager = Manager 
    {   
        state   :: State            -- ^ Estado do jogo.
    ,    pid    :: Int              -- ^ Id do jogador a controlar.
    ,    step   :: Int              -- ^ Número que indica quantas iterações o jogo já passou.
    ,    before :: Integer          -- ^ Intervalo de tempo entre jogadas.
    ,    delta  :: Integer          -- ^ Instante de tempo em que foi efetuada a última jogada.
    ,    delay  :: Integer          -- ^ Tempo decorrido desde a última jogada (milissegundos).
    } 

-- | Um jogador do tipo ghost.
data GhoState= GhoState 
    {
        ghostState :: PlayerState   -- ^ Estado do ghost.
    ,   ghostMode :: GhostMode      -- ^ Modo do ghost (Alive ou Dead).
    } deriving Eq

-- | Um par de coordenadas do tipo (/linha/,/coluna/).
type Coords = (Int,Int)

-- | Como é definido o estado de um jogador.
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 

-- | Tipos de boca de um pacman.
data Mouth 
   = Open       -- ^ Boca encontra-se aberta.
   | Closed     -- ^ Boca encontra-se fechada.
 deriving (Eq,Show)

-- | Tipos de modo de um pacman.
data PacMode 
   = Dying      -- ^ Pacman está no modo morto.
   | Mega       -- ^ Pacman está no modo mega.
   | Normal     -- ^ Pacman está no modo normal.
   deriving (Eq,Show)

-- | Tipos do fantasma.
data GhostMode 
   = Dead       -- ^ Fantasma encontra-se morto.
   | Alive      -- ^ Fantasma encontra-se vivo.
   deriving (Eq,Show)

-- | Tipos de comida.
data FoodType 
   = Big       -- ^ A comida é grande. 
   | Little    -- ^ A comida é pequena.
   deriving (Eq)

-- | Tipos de cor.
data Color 
   = Blue     -- ^ Azul.
   | Green    -- ^ Verde.
   | Purple   -- ^ Roxo.
   | Red      -- ^ Vermelho.
   | Yellow   -- ^ Amarelo.
   | None     -- ^ Nenhuma.
   deriving Eq 

-- | O tipo de jogadas que vão ocorrer no jogo.
data Play = Move Int Orientation deriving (Eq,Show)

-- | Tipo de instruções.
type Instructions = [Instruction]

-- | Tipo de instruções.
data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)


-- | Como é apresentado um State.
instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

-- | Como é apresentado o PacState.
instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

-- | Como é apresentado um jogador.
instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

-- | Como é apresentado o estado de um fantasma.
instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

-- | Como são apresentadas as diferentes comidas.
instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

-- | Como são apresentadas as várias peças de um labirinto.
instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


coloredString :: String -> Color -> String
coloredString x y = x
--    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
--    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
--    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
--    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
--    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
--    | otherwise =  "\x1b[0m" ++ x 

-- * Funções auxiliares usadas ao longo do trabalho.

-- | Coloca os jogadores num labirinto.
placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )

-- | Dá print de um labirinto.
printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

-- | Dá print das estatísticas de um jogo.
printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

-- | Obtém o ID de um jogador.
getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
-- | Obtém a pontuação de um jogador
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

-- | Altera as coordenadas de um jogador.
setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )

-- | Obtém o modo de um pacman.
getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

-- | Obtém o modo de um fantasma.
getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState (x,y,z,t,h,l) q ))= q

-- | Obtém o estado de um jogador.
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

-- | Obtém a orientação de um jogador.
getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

-- | Obtém a boca de um jogador.
getPlayerMouth :: Player -> Mouth 
getPlayerMouth (Pacman (PacState (x,y,z,t,h,l) q c d )) = c

-- | Troca uma peça de um labirinto pela peça que damos.
replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs

-- | Auxiliar da função 'replaceNElemInMaze'.
replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- | Obtém as coordenadas de um jogador.
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- | Verifica se um jogador é do tipo pacman.
isPacman :: Player -> Bool
isPacman (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) = True
isPacman _ = False

-- | Devolve um jogador consoante o ID que recebe.
getPlayer :: Int -> [Player] -> Player
getPlayer n (x:xs)
  |n== getPlayerID x = x
  |otherwise= getPlayer n xs

-- | Substitui um jogador num estado.
replacePlayer :: Player -> State -> State
replacePlayer player (State m (x1:xs) level) = (State m (replacePlayerAux player (x1:xs)) level) 

-- | Substitui um jogador numa lista de jogadores.
replacePlayerAux :: Player -> [Player] -> [Player]
replacePlayerAux _ [] = []
replacePlayerAux player (x1:xs)
     | getPlayerID player == (getPlayerID x1) = (player:xs)
     | otherwise= x1: replacePlayerAux player xs 

-- | Remove um jogador de um estado.
getPlayerFromList :: Int -> State -> Player 
getPlayerFromList n (State m (x:xs) l)
  | getPlayerID x ==n = x
  | otherwise = getPlayerFromList n (State m xs l)

-- | Remove um jogador de uma lista de jogadores.
removePlayer ::Player -> [Player] -> [Player]
removePlayer _ [] = []
removePlayer player (x:xs)
  |getPlayerID player == getPlayerID x = xs
  | otherwise= x : removePlayer player xs

-- | Ordena uma lista de jogadores para aparecer primeiro o pacman.
ordena :: [Player] -> [Player]
ordena [] = []
ordena (x:xs) 
  | isPacman x = (x:xs)
  | otherwise= (ordena xs) ++ [x]

-- | Verifica se um pacman está em modo mega.
isPacmanMega :: Player -> Bool
isPacmanMega (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) 
  | j== Mega = True
  | otherwise = False

-- | Verifica se o jogador é do tipo fantasma.
isGhost :: Player -> Bool
isGhost (Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)) = True
isGhost _ = False

-- | Obtém o tempo mega de um pacman.
getTimeMega :: Player -> Double
getTimeMega (Pacman (PacState (i,(b,c), x, y,z,l) h f j)) = h 

-- | A função 'whereIsPacman' utiliza a lista de jogadores e devolve o jogador do tipo Pacman.
whereIsPacman :: [Player] -> Player
whereIsPacman (a@(Pacman (PacState (i,(b,c), x, y,z,l) h f j)):xs) = a
whereIsPacman ((Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):xs) = whereIsPacman xs

-- | A função 'whereAreGhosts' utiliza a lista de jogadores e devolve apenas os do tipo Ghost.
whereAreGhosts :: [Player] -> [Player]
whereAreGhosts [] = []
whereAreGhosts (a@(Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) d)):xs) = a: whereAreGhosts xs
whereAreGhosts ((Pacman (PacState (i,(b,c), x, y,z,l) h f j)):xs) = whereAreGhosts xs

-- | Obtém o tempo meda do pacman de um determinado estado.
getTimeMegaFromState :: State -> Double
getTimeMegaFromState (State m ms l)= getTimeMega (whereIsPacman ms)

-- | Obtém a lista de jogadores do estado.
getPlayers :: State -> [Player]
getPlayers (State m ms l)= ms

-- | Obtém um labirinto de um estado.
getMaze :: State -> Maze
getMaze (State m ms l) = m

-- | Remove um jogador de um estado.
removePlayerFromState :: Player -> State -> State
removePlayerFromState a@(Ghost(GhoState (i1,(b1,c1), x1, y1,z1,l1) mode)) (State m ms l)= (State m (removeAux a ms) l)

-- | Função auxiliar da 'removePlayerFromState'
removeAux :: Player -> [Player] -> [Player]
removeAux x (h:t)
  |getPlayerID x== getPlayerID h= t
  | otherwise= h: removeAux x t

-- | Obtém a peça que se encontra à frente do jogador.
nextPiece :: Orientation -> Player -> Maze -> Piece 
nextPiece ori player m
   | ori== R && getPiece (a,b+1) m== Wall = Wall
   | ori== L && getPiece (a,b-1) m== Wall = Wall
   | ori== U && getPiece (a-1,b) m== Wall = Wall
   | ori== D && getPiece (a+1,b) m== Wall = Wall
   | otherwise= Empty
      where (a,b)= getPlayerCoords player

-- | Esta pequena função serve para obter a peça que está na posição que damos como argumento.
getPiece :: Coords -> Maze -> Piece
getPiece (b,c) l = ((l!!b)!!c)

-- | Muda a orientação do jogador em 180 graus.
rotate180Degrees :: Orientation -> Orientation
rotate180Degrees ori
   |ori==R = L
   |ori==L = R
   |ori==U = D
   |ori==D = U

-- | Muda a boca do jogador.
changeMouth :: Mouth -> Mouth
changeMouth b
   |b==Open = Closed
   |b==Closed = Open
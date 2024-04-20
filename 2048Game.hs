import Data.List     (elemIndices, intercalate, transpose)
import System.IO     (BufferMode(..), hSetBuffering, stdin)
import System.Random (randomRIO)

-- Direções possiveis
data Direction = Up_side | Left_side | Down_side | Right_side 
-- Definição do tabuleiro
type Board = [[Int]]

slideLeft :: Board -> Board
slideLeft = map slideLine
    where   slideLine [] = [] -- linha vazia
            slideLine [x] = [x] -- apenas um valor
            slideLine (x: y: xs)  -- dois ou mais valores
                | x == 0 = slideLine (y : xs) ++ [0] -- Se o primeiro elemento for zero, desliza-se a lista removendo o zero e adicionando-o ao final.
                | y == 0 = slideLine (x : xs) ++ [0] -- Se o segundo elemento for zero, desliza-se a lista removendo o zero e adicionando-o ao final, permitindo que os elementos se combinem.
                | x == y = (x + y) : slideLine xs ++ [0] -- Se os dois primeiros elementos forem iguais, combina-se esses dois elementos somando-os e adicionando o resultado ao início da lista resultante.
                | otherwise = x : slideLine (y : xs) -- Se nenhum dos casos anteriores for verdadeiro, mantém-se o primeiro elemento e continua-se a deslizar a lista.

-- Desliza os elementos
slide :: Direction -> Board -> Board
slide Up_side = transpose.slideLeft.transpose
slide Left_side = map reverse.slideLeft.map reverse
slide Down_side = transpose.map reverse.slideLeft.map reverse.transpose
slide Right_side  = slideLeft

-- Verifica se conseguiu vencer
win :: Board -> Bool
win x = any (elem 2048) x


stucked :: Board -> Bool
stucked b = all stucked_ b && all stucked_ (transpose b) 
  where stucked_ row = notElem 0 row && noNeighbors row
        noNeighbors [_] = True
        noNeighbors [ ] = True
        noNeighbors (x:y:xs)
          | x == y    = False
          | otherwise = noNeighbors (y:xs)

-- Retorna a posição dos lugares vazios
emptyPlaces :: Board -> [(Int, Int)]
emptyPlaces = concatMap (uncurry search).zip [0..3]
    where search line_index = zip(replicate 4 line_index).elemIndices 0

-- retorna a posição atualizada
updatePlace :: (Int, Int) -> Int -> Board -> Board
updatePlace (line, column) value = updateIndex (updateIndex (const value) column) line
    where updateIndex func index list = take index list ++ func (head $ drop index list) : tail (drop index list)


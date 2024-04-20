import Data.List     (elemIndices, intercalate, transpose)
import System.IO     (BufferMode(..), hSetBuffering, stdin)
import System.Random (randomRIO)

-- Direções possiveis
data Direction = Up_side | Left_side | Down_side | Right_side 
-- Definição do tabuleiro
type Board = [[Int]]

slide_left :: Board -> Board
slide_left = map slide_line
    where   slide_line [] = [] -- linha vazia
            slide_line [x] = [x] -- apenas um valor
            slide_line (x: y: xs)  -- dois ou mais valores
                | x == 0 = slide_line (y : xs) ++ [0] -- Se o primeiro elemento for zero, desliza-se a lista removendo o zero e adicionando-o ao final.
                | y == 0 = slide_line (x : xs) ++ [0] -- Se o segundo elemento for zero, desliza-se a lista removendo o zero e adicionando-o ao final, permitindo que os elementos se combinem.
                | x == y = (x + y) : slide_line xs ++ [0] -- Se os dois primeiros elementos forem iguais, combina-se esses dois elementos somando-os e adicionando o resultado ao início da lista resultante.
                | otherwise = x : slide_line (y : xs) -- Se nenhum dos casos anteriores for verdadeiro, mantém-se o primeiro elemento e continua-se a deslizar a lista.

slide :: Direction -> Board -> Board
slide Up_side = transpose.slide_left.transpose
slide Left_side = map reverse.slide_left.map reverse
slide Down_side = transpose.map reverse.slide_left.map reverse.transpose
slide Right_side  = slide_left


win :: Board -> Bool
win x = any (elem 2048) x

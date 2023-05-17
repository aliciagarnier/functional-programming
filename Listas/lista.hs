-- Algumas funções que trabalham com listas.
import Data.Char 
fat x = x * fat(x-1)

getImpar :: [Int] -> [Int]
getImpar [] = [] 
getImpar (a:x) -- esse parenteses serve parra dizer que isso é uma única coisa
    | a `mod` 2 /= 0 = a : getImpar x
    | otherwise = getImpar x


 -- para percorrer a lista com recursão utilizamos da técnica de separar cabeça e cauda, chamando a recursão para a cauda da lista, até atingirmos o caso base onde a cauda é a lista vazia, ou seja, cauda de um elemento.
    
insereOrdenado :: Int -> [Int] -> [Int]
insereOrdenado y [] = [y] -- Sem esse caso base, na situação em que o valor a ser inserido é o mair de todos, ele não é inserido.
insereOrdenado y (a:x)
    | y < a = y:a:x
    | otherwise =  a : insereOrdenado y x

ordenaLista :: [Int] -> [Int]
ordenaLista [] = [] 
ordenaLista (a:b) = insereOrdenado a (ordenaLista b)

ordenaListaLista :: [Int]-> [[Int]] -> [[Int]]
ordenaListaLista y [] = [y]
ordenaListaLista y (a:b)
    | y < a = y:a:b
    | otherwise = a : ordenaListaLista y b


listaDobro :: [Int] -> [Int]
listaDobro [] = [] 
listaDobro(a:b) = (a*2) : listaDobro b

maiorLista :: [Int] -> Int
maiorLista [x] = x
maiorLista (a:b) 
    | a > maiorLista b = a
    | otherwise = maiorLista b
    

stringInteiro:: [Char] -> [Int]
stringInteiro [] = [] 
stringInteiro (a:x) = ord a:stringInteiro x


duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate x y  = x ++ duplicate x (y-1)

pushRight :: String -> Int -> String
pushRight s n 
    |n <= length s = s
    | otherwise = ">" ++ pushRight s (n-1)


separa :: [Int] -> ([Int], [Int])
separa [] = ([], [])
separa (a:b) 
    | a `mod` 2 == 0 = (a:fst(separa b), snd(separa b))
    | otherwise = (fst(separa b), a:snd(separa b))

converte :: [Int] -> [Char]
converte [] = ""
converte (a:x) = chr(a+64) : converte x 

conta :: [Char] -> [Char] -> Int
conta [] _ = 0 
conta (a:b) c 
    | [a] == c = 1 + conta b c
    | otherwise = conta b c

purifica :: [Int] -> [Int]
purifica [x] = [x] 
purifica (a:b:c) 
        | a == b = a : purifica c
        | otherwise = a:b : purifica (b:c)


charToNum :: Char -> Int
charToNum x
    |isDigit x = ord x - ord '0' 
    | otherwise = -1 


maiusculo :: Char -> Char
maiusculo x 
    | isLower x = chr(ord x - (ord 'a' - ord 'A'))

-- Exemplo de um banco de dadoss
vendas :: Int -> Int 
vendas 0 = 0 
vendas 1 = 41
vendas 2 = 72 
vendas 3 = 48 
vendas 4 = 0 
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

-- Função que define o perido de recursão.

periodo:: Int
periodo = 7

-- Função que retorna o dia que obteve determinada quantidade de vendas.

diaVenda:: Int -> Int -> Int
diaVenda 0 _ = 0 
diaVenda d v
    | v == vendas d = d 
    | otherwise = diaVenda (d - 1) v 

-- Função que retorna o total de vendas.

totalVendas :: Int -> Int
totalVendas 0 = vendas 0 
totalVendas x = totalVendas (x-1) + vendas x

-- Função que retorna quantas vendas superam determinado valor.

quantasVendas:: Int -> Int -> Int
quantasVendas 0 _ = 0 
quantasVendas x y 
    | vendas x > y = quantasVendas (x-1) y + 1   
    | otherwise = quantasVendas (x-1) y 

-- Soma a quantidade de vendas de todos os dias que são pares em um dado período.

somaPar :: Int -> Int 
somaPar 0 = vendas 0 
somaPar x
    | x `mod` 2 == 0 = somaPar(x-1) + vendas x 
    | otherwise = somaPar(x-1)


-- Algumas possibilidades de modularização para esse tipo de função.

par :: Int -> Bool
par x = x `mod` 2 == 0

impar :: Int -> Bool
impar x = not(par x)


maior :: Int -> Int -> Int
maior x y 
    | x > y = x 
    | otherwise = y 


-- Ainda com a ideia de modularização para o problema de encontrar a maior venda em um período. 

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0 
maiorVenda x
    | vendas x > maiorVenda(x-1) = vendas x
    | otherwise = maiorVenda(x-1)

-- Utilizando a função maior. 

maiorVenda1 :: Int -> Int 
maiorVenda1 0 =  vendas 0 
maiorVenda1 x = maior (vendas x) (maiorVenda1(x-1))

-- Função que retorna o dia que obteve a melhor venda. 

melhorDiaVenda :: Int -> Int -> Int
melhorDiaVenda 0 _ = vendas 0 
melhorDiaVenda x y 
    | vendas x == y = x
    | otherwise = melhorDiaVenda (x-1) y


-- Função que retorna uma lista de lista. Cada lista interna contém dia e venda. 

listaDiaVendas :: Int -> [[Int]]
listaDiaVendas 0 = [] -- lista vazia caso base
listaDiaVendas x = [x, vendas x] : listaDiaVendas(x-1)


-- Recebe o periodo de vendas e retorna uma lista de tuplas (Int,Int) = (Dia, Venda).

listaTuplaVendas :: Int -> [(Int, Int)] 
listaTuplaVendas 0 = [] 
listaTuplaVendas x = listaTuplaVendas (x-1) ++ [(x, vendas x)]

-- Função que retorna True somente se não há nenhum dia no período em que o número de vendas foi zero.

nozeroinPeriod :: Int -> Bool
nozeroinPeriod 0 = True
nozeroinPeriod x
    | vendas x == 0 =  False
    | otherwise = nozeroinPeriod(x-1)


-- Retorna a lista de todos os dias em que as vendas foram de zero unidades.

zerosinPeriod :: Int -> [Int]
zerosinPeriod 0 = []
zerosinPeriod x 
    | vendas x == 0 = x : zerosinPeriod(x-1)
    | otherwise = zerosinPeriod (x-1)

-- Retorna uma lista com valores inferiores a um parâmetro fornecido. entrada: periodo <valor>.

inferiores :: Int -> Int -> [Int]
inferiores 0 _ = []
inferiores x y  
    | vendas x < y = x : inferiores(x-1) y 
    | otherwise = inferiores (x-1) y 

-- Quantas vendas foram inferiores a um valor fornecido dentro de um intervalo de dias dado.

howManyLess :: Int -> Int -> Int -> Int 
howManyLess 0 _ _ = 0 
howManyLess a b c

    | c >= b && vendas b < a = 1 + howManyLess a b (c-1)
    | otherwise = howManyLess a b (c-1)

-- Retorna uma lista com os dias de um periodo de vendas.

listaDia :: Int -> [Int]
listaDia 0 = []
listaDia x = x : listaDia(x-1)

-- Retorna uma lista com as quantidades vendidas em um período.

listaVendas :: Int -> [Int]
listaVendas 0 = []
listaVendas x = vendas x : listaVendas(x-1)

-- Calcula o total de vendas ao se receber uma lista de tuplas.

criaListaTupla :: [Int] -> [Int] -> [(Int, Int)]
criaListaTupla [] [] = [] 
criaListaTupla (a:b) (x:y) = (a,x) : criaListaTupla b y 

-- Calcula o total de vendas de uma lista de tuplas.

totalVendasB :: [(Int, Int)] -> Int
totalVendasB [] = 0 
totalVendasB (a:x) = snd a + totalVendasB x 




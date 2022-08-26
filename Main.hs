-- Nome: Igor Hipólito Vieira

{-
1. Escreva uma função para o cálculo dos números da sequência de Fibonacci, utilizando
Haskell.
-}
ffib :: Int -> Int
ffib 0 = 0
ffib 1 = 1
ffib x = ffib (x -1) + ffib (x -2)

{-
2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor
Comum (MDC) de Euclides publicado por volta do ano 300 AC. Podemos simplificar este
algoritmo dizendo que dados dois inteiros A e B, o MDC entre eles será dado pelo valor
absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva
uma função para o cálculo do MDC entre dois números inteiros positivos, usando o
algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
-}
mdc :: Int -> Int -> Int
mdc a b
  | b == 0 = a
  | a == 0 = b
  | a > b = mdc b (mod a b)
  | b > a = mdc a (mod b a)

{-
3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos
deste número. Exemplo: dado 1234 a função deverá devolver 10. Utilizando Haskell e
recursividade.
-}
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos num = (mod num 10) + (somaDigitos (div num 10))

{-
4. Escreva uma função que devolva a soma de todos os números menores que 10000 que
sejam múltiplos de 3 ou 5.
-}
somaMenores10000 :: Int
somaMenores10000 = sum [x | x <- [1 .. 9999], (mod x 3) == 0 || (mod x 5) == 0]

{-
5. Escreva uma função que, recebendo uma lista de inteiros, apresente a diferença entre a
soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade.
-}
diferencaQuadSoma :: [Int] -> Int
diferencaQuadSoma li = (quadradoSoma li) - (somaQuadrados li)
  where
    quadradoSoma li = (somaLista 0 li) ^ 2
    somaQuadrados li = somaLista 0 (quadradoLista li)
    quadradoLista [] = []
    quadradoLista (primeiro : resto) = primeiro ^ 2 : (quadradoLista resto)
    somaLista acumulador [] = acumulador
    somaLista acumulador (primeiro : resto) = (+) primeiro (somaLista acumulador resto)

{-
7. Nem só de Fibonacci vivem os exemplos de recursão. Escreva uma função que devolva
todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores
que um inteiro dado.
-}
seqLuc :: Int -> [Int]
seqLuc n = reverse (_seqLuc n)
  where
    _seqLuc 0 = [2]
    _seqLuc 1 = [1, 2]
    _seqLuc n = (head (_seqLuc (n - 1)) + head (_seqLuc (n - 2))) : _seqLuc (n - 1)

{-
8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3]
devolva [3,2,1].
-}
aoContrario :: [Int] -> [Int]
aoContrario lista = contr lista []
  where
    contr [] novaLista = novaLista
    contr (primeiro : resto) novaLista = contr resto (primeiro : novaLista)

{-
9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o
produto destes valores sem usar o operador de multiplicação.
-}
somaRecursiva :: Int -> Int -> Int
somaRecursiva a 1 = a
somaRecursiva a b = a + somaRecursiva a (b - 1)

{-
10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o
comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule
o comprimento de uma lista.
-}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (primeiro : resto) = (+) 1 (comprimento resto)

main = do
  putStrLn $ "Func. 1: entrada 9; resultado: " ++ show (ffib 9)
  putStrLn $ "Func. 2: entrada 12 6; resultado: " ++ show (mdc 12 6)
  putStrLn $ "Func. 3: entrada 2424; resultado: " ++ show (somaDigitos 2424)
  putStrLn $ "Func. 4: resultado: " ++ show somaMenores10000
  putStrLn $ "Func. 5: entrada [1, 2]; resultado: " ++ show (diferencaQuadSoma [1, 2])
  putStrLn $ "Func. 7: entrada 13; resultado: " ++ show (seqLuc 13)
  putStrLn $ "Func. 8: entrada [2, 3, 4, 5]; resultado: " ++ show (aoContrario [2, 3, 4, 5])
  putStrLn $ "Func. 9: entrada 2 6; resultado: " ++ show (somaRecursiva 2 6)
  putStrLn $ "Func. 10: entrada [5, 6, 7, 8, 9]; resultado: " ++ show (comprimento [5, 6, 7, 8, 9])

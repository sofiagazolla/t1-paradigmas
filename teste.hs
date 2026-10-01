module Main where

import Data.List (isPrefixOf, nub, permutations, transpose)

type Tabuleiro = [[Int]]
type Pistas = [Int]


visiveis :: [Int] -> Int
visiveis = snd . foldl passo (0, 0)
  where
    passo (maior, quantidade) altura
      | altura > maior = (altura, quantidade + 1)
      | otherwise = (maior, quantidade)


satisfazPista :: Int -> [Int] -> Bool
satisfazPista 0 _ = True
satisfazPista pista xs =
  visiveis xs == pista


linhaValida :: Int -> Int -> [Int] -> Bool
linhaValida pistaInicio pistaFim xs =
  satisfazPista pistaInicio xs
    && satisfazPista pistaFim (reverse xs)


valoresLinha :: Int -> Int -> [Int]
valoresLinha n maxAltura =
  replicate quantidadeVazios 0 ++ [1 .. maxAltura]
  where
    quantidadeVazios =
      n - maxAltura


permutacoesValidas :: Int -> Int -> Int -> Int -> [[Int]]
permutacoesValidas n maxAltura pistaInicio pistaFim =
  filter
    (linhaValida pistaInicio pistaFim)
    (nub (permutations (valoresLinha n maxAltura)))


gerarOpcoes :: Int -> Int -> Pistas -> Pistas -> [[[Int]]]
gerarOpcoes n maxAltura pistasInicio pistasFim =
  zipWith
    (permutacoesValidas n maxAltura)
    pistasInicio
    pistasFim


colunasAindaPossiveis :: Tabuleiro -> [[[Int]]] -> Bool
colunasAindaPossiveis tabuleiro opcoesColunas =
  and $
    zipWith
      colunaPossivel
      (transpose tabuleiro)
      opcoesColunas
  where
    colunaPossivel prefixo opcoes =
      any (prefixo `isPrefixOf`) opcoes


resolver ::
  Int ->
  Int ->
  Pistas ->
  Pistas ->
  Pistas ->
  Pistas ->
  Maybe Tabuleiro

resolver n maxAltura topo baixo esquerda direita
  | not entradaValida = Nothing
  | otherwise = buscar [] opcoesLinhas
  where
    opcoesLinhas =
      gerarOpcoes
        n
        maxAltura
        esquerda
        direita

    opcoesColunas =
      gerarOpcoes
        n
        maxAltura
        topo
        baixo

    entradaValida =
      maxAltura >= 1
        && maxAltura <= n
        && all
          ((== n) . length)
          [topo, baixo, esquerda, direita]
        && all
          pistaValida
          (topo ++ baixo ++ esquerda ++ direita)

    pistaValida pista =
      pista >= 0 && pista <= maxAltura

    buscar :: Tabuleiro -> [[[Int]]] -> Maybe Tabuleiro

    buscar tabuleiro [] =
      Just tabuleiro

    buscar tabuleiro (opcoesAtuais : restantes) =
      tentar opcoesAtuais
      where
        tentar [] =
          Nothing

        tentar (linha : outrasLinhas)
          | colunasAindaPossiveis
              novoTabuleiro
              opcoesColunas =
              case buscar novoTabuleiro restantes of
                Just solucao ->
                  Just solucao

                Nothing ->
                  tentar outrasLinhas

          | otherwise =
              tentar outrasLinhas

          where
            novoTabuleiro =
              tabuleiro ++ [linha]


mostrarPista :: Int -> String
mostrarPista 0 = "."
mostrarPista x = show x


mostrarCelula :: Int -> String
mostrarCelula 0 = "X"
mostrarCelula x = show x


imprimirPuzzle ::
  Pistas ->
  Pistas ->
  Pistas ->
  Pistas ->
  Tabuleiro ->
  IO ()

imprimirPuzzle topo baixo esquerda direita tabuleiro = do
  putStrLn $
    "    " ++ unwords (map mostrarPista topo)

  putStrLn "   +-------------+"

  mapM_
    imprimirLinha
    (zip3 esquerda tabuleiro direita)

  putStrLn "   +-------------+"

  putStrLn $
    "    " ++ unwords (map mostrarPista baixo)

  where
    imprimirLinha (pistaEsquerda, linha, pistaDireita) =
      putStrLn $
        mostrarPista pistaEsquerda
          ++ " | "
          ++ unwords (map mostrarCelula linha)
          ++ " | "
          ++ mostrarPista pistaDireita


n :: Int
n = 6

maxAltura :: Int
maxAltura = 6


topo :: Pistas
topo = [3, 1, 2, 3, 4, 0]

baixo :: Pistas
baixo = [2, 3, 0, 0, 0, 0]

esquerda :: Pistas
esquerda = [2, 2, 0, 0, 0, 0]

direita :: Pistas
direita = [5, 0, 4, 0, 6, 0]


main :: IO ()
main = do
  case resolver
    n
    maxAltura
    topo
    baixo
    esquerda
    direita of

    Nothing ->
      putStrLn "Nenhuma solucao foi encontrada."

    Just solucao -> do
      putStrLn "Solucao encontrada:"
      putStrLn ""

      imprimirPuzzle
        topo
        baixo
        esquerda
        direita
        solucao
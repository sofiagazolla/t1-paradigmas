module Main where

import Data.List (isPrefixOf, permutations, transpose)

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
satisfazPista pista xs = visiveis xs == pista


linhaValida :: Int -> Int -> [Int] -> Bool
linhaValida pistaInicio pistaFim xs =
  satisfazPista pistaInicio xs
    && satisfazPista pistaFim (reverse xs)


permutacoesValidas :: Int -> Int -> Int -> [[Int]]
permutacoesValidas n pistaInicio pistaFim =
  filter
    (linhaValida pistaInicio pistaFim)
    (permutations [1 .. n])


gerarOpcoes :: Int -> Pistas -> Pistas -> [[[Int]]]
gerarOpcoes n pistasInicio pistasFim =
  zipWith
    (permutacoesValidas n)
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


resolver :: Int -> Pistas -> Pistas -> Pistas -> Pistas -> Maybe Tabuleiro
resolver n topo baixo esquerda direita
  | not entradaValida = Nothing
  | otherwise = buscar [] opcoesLinhas
  where
    opcoesLinhas =
      gerarOpcoes n esquerda direita

    opcoesColunas =
      gerarOpcoes n topo baixo

    entradaValida =
      all ((== n) . length) [topo, baixo, esquerda, direita]
        && all
          pistaValida
          (topo ++ baixo ++ esquerda ++ direita)

    pistaValida pista =
      pista >= 0 && pista <= n

    buscar :: Tabuleiro -> [[[Int]]] -> Maybe Tabuleiro
    buscar tabuleiro [] =
      Just tabuleiro

    buscar tabuleiro (opcoesAtuais : restantes) =
      tentar opcoesAtuais
      where
        tentar [] =
          Nothing

        tentar (linha : outrasLinhas)
          | colunasAindaPossiveis novoTabuleiro opcoesColunas =
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


imprimirPuzzle :: Pistas -> Pistas -> Pistas -> Pistas -> Tabuleiro -> IO ()
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
          ++ unwords (map show linha)
          ++ " | "
          ++ mostrarPista pistaDireita


n :: Int
n = 6

topo :: Pistas
topo = [4, 1, 2, 2, 3, 2]

baixo :: Pistas
baixo = [1, 3, 5, 2, 4, 2]

esquerda :: Pistas
esquerda = [2, 3, 3, 4, 2, 1]

direita :: Pistas
direita = [2, 4, 2, 3, 1, 4]


main :: IO ()
main = do
  case resolver n topo baixo esquerda direita of
    Nothing ->
      putStrLn "Nenhuma solucao foi encontrada."

    Just solucao -> do
      putStrLn "Solucao encontrada:"
      putStrLn ""
      imprimirPuzzle topo baixo esquerda direita solucao
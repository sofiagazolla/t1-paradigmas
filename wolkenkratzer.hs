module Main where

import Data.List (permutations, transpose, isPrefixOf)

-- Um tabuleiro e uma lista de pistas sao representados por listas de inteiros.
type Tabuleiro = [[Int]]
type Pistas = [Int]

-- Conta quantos predios sao visiveis ao observar uma linha da esquerda
-- para a direita. Um predio so e visivel se for maior que todos os
-- predios que apareceram antes dele.
visiveis :: [Int] -> Int
visiveis = snd . foldl passo (0, 0)
  where
    passo (maior, quantidade) altura
      | altura > maior = (altura, quantidade + 1)
      | otherwise      = (maior, quantidade)

-- Verifica se uma lista de alturas satisfaz uma pista.
-- O valor 0 representa uma pista ausente e, portanto, aceita qualquer linha.
satisfazPista :: Int -> [Int] -> Bool
satisfazPista 0 _      = True
satisfazPista pista xs = visiveis xs == pista

-- Verifica simultaneamente as duas pistas de uma linha ou coluna.
-- A primeira pista observa a lista na ordem normal e a segunda observa
-- a mesma lista no sentido contrario.
linhaValida :: Int -> Int -> [Int] -> Bool
linhaValida pistaInicio pistaFim xs =
  satisfazPista pistaInicio xs &&
  satisfazPista pistaFim (reverse xs)

-- Gera todas as permutacoes de 1 ate N que respeitam as duas pistas.
-- Como cada permutacao contem todos os numeros uma unica vez, a regra de
-- nao repetir alturas na linha/coluna ja e automaticamente satisfeita.
permutacoesValidas :: Int -> Int -> Int -> [[Int]]
permutacoesValidas n pistaInicio pistaFim =
  filter (linhaValida pistaInicio pistaFim) (permutations [1 .. n])

-- Para cada linha do tabuleiro, gera previamente todas as possibilidades
-- que satisfazem suas pistas da esquerda e da direita.
gerarOpcoes :: Int -> Pistas -> Pistas -> [[[Int]]]
gerarOpcoes n pistasInicio pistasFim =
  zipWith (permutacoesValidas n) pistasInicio pistasFim

-- Verifica se cada coluna parcial do tabuleiro ainda pode ser completada
-- por pelo menos uma coluna valida.
--
-- Exemplo: se uma coluna parcial e [2,4], ela so e aceita se existir alguma
-- coluna completa valida que comece por [2,4]. Isso elimina cedo escolhas
-- que nunca poderiam formar uma solucao.
colunasAindaPossiveis :: Tabuleiro -> [[[Int]]] -> Bool
colunasAindaPossiveis tabuleiro opcoesColunas =
  and (zipWith colunaPossivel (transpose tabuleiro) opcoesColunas)
  where
    colunaPossivel prefixo opcoes =
      any (prefixo `isPrefixOf`) opcoes

-- Resolve o puzzle por backtracking.
--
-- O algoritmo tenta uma possibilidade para a primeira linha, verifica as
-- colunas, tenta a proxima linha e assim por diante. Se em algum momento
-- nenhuma coluna puder mais ser completada, volta para a escolha anterior.
resolver :: Int -> Pistas -> Pistas -> Pistas -> Pistas -> Maybe Tabuleiro
resolver n topo baixo esquerda direita
  | not entradaValida = Nothing
  | otherwise         = buscar [] opcoesLinhas
  where
    -- Opcoes possiveis para cada linha e para cada coluna.
    opcoesLinhas  = gerarOpcoes n esquerda direita
    opcoesColunas = gerarOpcoes n topo baixo

    entradaValida =
      all ((== n) . length) [topo, baixo, esquerda, direita] &&
      all pistaValida (topo ++ baixo ++ esquerda ++ direita)

    pistaValida pista = pista >= 0 && pista <= n

    -- Caso base: se nao existem mais linhas para preencher, o tabuleiro
    -- completo e uma solucao.
    buscar :: Tabuleiro -> [[[Int]]] -> Maybe Tabuleiro
    buscar tabuleiro [] = Just tabuleiro

    -- Tenta todas as alternativas da linha atual.
    buscar tabuleiro (opcoesAtuais : restantes) =
      tentar opcoesAtuais
      where
        tentar [] = Nothing
        tentar (linha : outrasLinhas)
          | colunasAindaPossiveis novoTabuleiro opcoesColunas =
              case buscar novoTabuleiro restantes of
                Just solucao -> Just solucao
                Nothing      -> tentar outrasLinhas
          | otherwise = tentar outrasLinhas
          where
            novoTabuleiro = tabuleiro ++ [linha]

-- Converte uma pista para texto. Zero e exibido como ponto, indicando
-- que aquela pista nao foi fornecida.
mostrarPista :: Int -> String
mostrarPista 0 = "."
mostrarPista x = show x

-- Exibe o tabuleiro junto com as pistas externas.
imprimirPuzzle :: Pistas -> Pistas -> Pistas -> Pistas -> Tabuleiro -> IO ()
imprimirPuzzle topo baixo esquerda direita tabuleiro = do
  putStrLn $ "    " ++ unwords (map mostrarPista topo)
  putStrLn "   +-------------+"
  mapM_ imprimirLinha (zip3 esquerda tabuleiro direita)
  putStrLn "   +-------------+"
  putStrLn $ "    " ++ unwords (map mostrarPista baixo)
  where
    imprimirLinha (pistaEsquerda, linha, pistaDireita) =
      putStrLn $
        mostrarPista pistaEsquerda ++ " | " ++
        unwords (map show linha) ++
        " | " ++ mostrarPista pistaDireita

-- Exemplo 6x6 baseado em um puzzle Wolkenkratzer publicado no site Janko.
-- Para testar outro puzzle, basta alterar estas quatro listas.
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
  putStrLn "Wolkenkratzer - resolvedor em Haskell"
  putStrLn ""

  case resolver n topo baixo esquerda direita of
    Nothing -> putStrLn "Nenhuma solucao foi encontrada."
    Just solucao -> do
      putStrLn "Solucao encontrada:"
      putStrLn ""
      imprimirPuzzle topo baixo esquerda direita solucao

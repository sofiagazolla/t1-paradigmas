from itertools import permutations


# Conta quantos prédios são visíveis ao observar uma linha
# da esquerda para a direita.
def visiveis(linha):
    maior = 0
    quantidade = 0

    for altura in linha:
        if altura > maior:
            maior = altura
            quantidade += 1

    return quantidade


# Verifica se uma linha satisfaz uma pista.
# O valor 0 representa uma pista ausente.
def satisfaz_pista(pista, linha):
    if pista == 0:
        return True

    return visiveis(linha) == pista


# Verifica simultaneamente as pistas dos dois lados
# de uma linha ou coluna.
def linha_valida(pista_inicio, pista_fim, linha):
    return (
        satisfaz_pista(pista_inicio, linha)
        and satisfaz_pista(pista_fim, linha[::-1])
    )


# Gera todas as permutações de 1 até N que respeitam
# as duas pistas.
def permutacoes_validas(n, pista_inicio, pista_fim):
    resultado = []

    for permutacao in permutations(range(1, n + 1)):
        linha = list(permutacao)

        if linha_valida(pista_inicio, pista_fim, linha):
            resultado.append(linha)

    return resultado


# Para cada linha/coluna, gera previamente todas as possibilidades
# que satisfazem suas pistas.
def gerar_opcoes(n, pistas_inicio, pistas_fim):
    opcoes = []

    for pista_inicio, pista_fim in zip(pistas_inicio, pistas_fim):
        opcoes.append(
            permutacoes_validas(n, pista_inicio, pista_fim)
        )

    return opcoes


# Transpõe uma matriz.
# Exemplo:
#
# [[1, 2],
#  [3, 4]]
#
# vira:
#
# [[1, 3],
#  [2, 4]]
def transpor(tabuleiro):
    return [list(coluna) for coluna in zip(*tabuleiro)]


# Verifica se cada coluna parcial ainda pode ser completada
# por pelo menos uma coluna válida.
def colunas_ainda_possiveis(tabuleiro, opcoes_colunas):

    colunas_parciais = transpor(tabuleiro)

    for prefixo, opcoes in zip(colunas_parciais, opcoes_colunas):

        coluna_possivel = False

        for opcao in opcoes:
            # Equivalente ao isPrefixOf do Haskell:
            # verifica se prefixo corresponde ao começo da opção.
            if opcao[:len(prefixo)] == prefixo:
                coluna_possivel = True
                break

        if not coluna_possivel:
            return False

    return True


# Resolve o puzzle usando backtracking.
def resolver(n, topo, baixo, esquerda, direita):

    # Verifica se a entrada é válida.
    if not (
        len(topo) == n
        and len(baixo) == n
        and len(esquerda) == n
        and len(direita) == n
    ):
        return None

    todas_pistas = topo + baixo + esquerda + direita

    for pista in todas_pistas:
        if pista < 0 or pista > n:
            return None

    # Opções possíveis para cada linha.
    opcoes_linhas = gerar_opcoes(
        n,
        esquerda,
        direita
    )

    # Opções possíveis para cada coluna.
    opcoes_colunas = gerar_opcoes(
        n,
        topo,
        baixo
    )

    # Função interna de backtracking.
    def buscar(tabuleiro, indice_linha):

        # Caso base:
        # todas as linhas foram preenchidas.
        if indice_linha == n:
            return tabuleiro

        # Testa cada possibilidade para a linha atual.
        for linha in opcoes_linhas[indice_linha]:

            novo_tabuleiro = tabuleiro + [linha]

            # Só continua se todas as colunas parciais
            # ainda puderem formar alguma solução.
            if colunas_ainda_possiveis(
                novo_tabuleiro,
                opcoes_colunas
            ):
                solucao = buscar(
                    novo_tabuleiro,
                    indice_linha + 1
                )

                if solucao is not None:
                    return solucao

        # Nenhuma opção funcionou.
        return None

    return buscar([], 0)


# Converte uma pista para texto.
# Zero é exibido como ponto.
def mostrar_pista(pista):
    if pista == 0:
        return "."

    return str(pista)


# Exibe o puzzle junto com as pistas externas.
def imprimir_puzzle(topo, baixo, esquerda, direita, tabuleiro):

    print("    " + " ".join(map(mostrar_pista, topo)))

    # Ajusta o tamanho da borda ao tamanho do tabuleiro.
    largura = 2 * len(topo) + 1

    print("   +" + "-" * largura + "+")

    for pista_esquerda, linha, pista_direita in zip(
        esquerda,
        tabuleiro,
        direita
    ):
        print(
            mostrar_pista(pista_esquerda)
            + " | "
            + " ".join(map(str, linha))
            + " | "
            + mostrar_pista(pista_direita)
        )

    print("   +" + "-" * largura + "+")

    print("    " + " ".join(map(mostrar_pista, baixo)))


# ------------------------------------------------------
# Puzzle de exemplo
# ------------------------------------------------------

n = 6

topo = [4, 1, 2, 2, 3, 2]

baixo = [1, 3, 5, 2, 4, 2]

esquerda = [2, 3, 3, 4, 2, 1]

direita = [2, 4, 2, 3, 1, 4]


def main():

    print("Wolkenkratzer - resolvedor em Python")
    print()

    solucao = resolver(
        n,
        topo,
        baixo,
        esquerda,
        direita
    )

    if solucao is None:
        print("Nenhuma solução foi encontrada.")

    else:
        print("Solução encontrada:")
        print()

        imprimir_puzzle(
            topo,
            baixo,
            esquerda,
            direita,
            solucao
        )


if __name__ == "__main__":
    main()
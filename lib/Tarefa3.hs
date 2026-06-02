{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : José Manuel Peixoto Rocha <a106887@alunos.uminho.pt>
              David Jose Barbosa Alves <a107324@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where
import LI12425
import Tarefa2
import Data.List(delete)

{-|
Função principal de atualização do estado do jogo
-}
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo = jogoAtualizado
  where
    jogoComTorresAtualizadas = atualizaTorres tempo jogo
    jogoComInimigosAtualizados = atualizaInimigos tempo jogoComTorresAtualizadas
    jogoComPortaisAtualizados = atualizaPortais tempo jogoComInimigosAtualizados
    jogoAtualizado = jogoComPortaisAtualizados

{-|
Atualiza o estado das Torres (incluindo ataques a Inimigos)
-}
atualizaTorres :: Tempo -> Jogo -> Jogo
atualizaTorres tempo jogo = jogo { torresJogo = novasTorres, inimigosJogo = inimigosAtualizados }
  where
    novasTorres = map (atualizaTempoTorre tempo) (torresJogo jogo)
    inimigosAlvo = inimigosJogo jogo
    inimigosAtualizados = foldl (\inims torre -> disparaTorre torre inims) inimigosAlvo novasTorres

{-|
Atualiza o tempo restante de recarga de cada Torre
-}
atualizaTempoTorre :: Tempo -> Torre -> Torre
atualizaTempoTorre tempo torre@(Torre {tempoTorre = t})
  | t > 0     = torre {tempoTorre = t - tempo}
  | otherwise = torre

{-|
Função que dispara a torre, aplicando as regras de rajada e ciclo de tempo
-}
disparaTorre :: Torre -> [Inimigo] -> [Inimigo]
disparaTorre torre inimigos
  | tempoTorre torre <= 0 =
      let inimigosNoRaio = take (rajadaTorre torre) (inimigosNoAlcance torre inimigos)  
          -- Aplica dano a cada inimigo no alcance da torre
          inimigosComDano = map (atingeInimigo torre) inimigosNoRaio
          -- Remove os inimigos atingidos usando a função auxiliar
          inimigosRestantes = removeAtingidos inimigosNoRaio inimigos
      in inimigosComDano ++ inimigosRestantes
  | otherwise = inimigos

{-|
Função auxiliar que remove os inimigos atingidos da lista de todos os inimigos
-}
removeAtingidos :: [Inimigo] -> [Inimigo] -> [Inimigo]
removeAtingidos atingidos todos = foldl (\acc inimigo -> delete inimigo acc) todos atingidos

{-|
Atualiza os créditos da base com base no butim dos inimigos derrotados
-}
atualizaCreditos :: [Inimigo] -> Base -> Base
atualizaCreditos inimigos base =
    let creditosAdicionais = sum (map butimInimigo (filter (\inimigo -> vidaInimigo inimigo <= 0) inimigos))
    in base { creditosBase = creditosBase base + creditosAdicionais }

{-|
Atualiza o estado dos inimigos e os créditos da base
-}
atualizaInimigos :: Tempo -> Jogo -> Jogo
atualizaInimigos tempo jogo =
    let basePos = posicaoBase (baseJogo jogo)
        mapa = mapaJogo jogo
        torres = torresJogo jogo
        -- Atualiza os inimigos
        inimigosAtualizados = map (atualizaInimigo tempo mapa torres basePos) (inimigosJogo jogo)
        -- Filtra apenas inimigos vivos (vida > 0)
        inimigosVivos = filter (\inimigo -> vidaInimigo inimigo > 0) inimigosAtualizados
        -- Atualiza os créditos da base com base nos inimigos derrotados
        baseAtualizada = atualizaCreditos inimigosAtualizados (baseJogo jogo)
        -- Calcula o dano da base somente para os inimigos que chegaram à base
        inimigosNaBase = filter (\inimigo -> vidaInimigo inimigo > 0 && chegouBase basePos inimigo) inimigosAtualizados
        danoTotal = sum (map (danoSeNaBase basePos) inimigosNaBase)
        novaVidaBase = vidaBase (baseJogo jogo) - danoTotal
    in jogo
        { inimigosJogo = inimigosVivos,  -- Atualiza com apenas inimigos vivos
          baseJogo = baseAtualizada { vidaBase = novaVidaBase }
        }

{-|
Atualiza o estado de um inimigo com base no tempo, mapa, torres e posição da base
-}
atualizaInimigo :: Tempo -> Mapa -> [Torre] -> Posicao -> Inimigo -> Inimigo
atualizaInimigo tempo mapa torres basePos inimigo
  | chegouBase basePos inimigo = inimigo
  | otherwise = moveInimigo tempo mapa torres inimigo

{-|
Move o inimigo com base no tempo, mapa, torres e efeitos dos projéteis, ajustando posição e direção
-}
moveInimigo :: Tempo -> Mapa -> [Torre] -> Inimigo -> Inimigo
moveInimigo tempo mapa torres inimigo@(Inimigo {velocidadeInimigo = v, posicaoInimigo = (x, y), direcaoInimigo = dir, projeteisInimigo = proj}) =
    let
        -- Verifica se está próximo da borda do bloco atual com uma margem maior
        (blocoX, blocoY) = (floor x, floor y)
        estaPertoDaBorda = case dir of
            Norte -> y - fromIntegral blocoY < 0.5
            Sul   -> fromIntegral (blocoY + 1) - y < 1.0
            Este  -> fromIntegral (blocoX + 1) - x < 1.0
            Oeste -> x - fromIntegral blocoX < 0.1

        -- Verifica se o próximo bloco na direção atual é Terra
        proximoTerreno = case dir of
            Norte -> acharTerreno mapa (blocoX, blocoY - 1)
            Sul   -> acharTerreno mapa (blocoX, blocoY + 1)
            Este  -> acharTerreno mapa (blocoX + 1, blocoY)
            Oeste -> acharTerreno mapa (blocoX - 1, blocoY)

        -- Decide se precisa de mudar a direção
        precisaMudarDirecao = estaPertoDaBorda && proximoTerreno /= Just Terra

        -- Calcula nova direção se necessário
        novaDirecao = if precisaMudarDirecao
                     then escolheNovaDirecao mapa (x, y) dir
                     else dir

        -- Calcula velocidade baseada nos efeitos
        velocidade
          | any (\p -> tipoProjetil p == Gelo) proj = 0
          | any (\p -> tipoProjetil p == Resina) proj = 0.001
          | otherwise = 0.002 * v

        -- Calcula nova posição
        novaPosicao = if any (\p -> tipoProjetil p == Gelo) proj
                     then (x, y)
                     else case novaDirecao of
                          Norte -> (x, y - velocidade)
                          Sul   -> (x, y + velocidade)
                          Este  -> (x + velocidade, y)
                          Oeste -> (x - velocidade, y)

        -- Verifica se o inimigo está no alcance de alguma torre
        inimigoEstaNoAlcance = any (\torre -> not (null (inimigosNoAlcance torre [inimigo]))) torres

        -- Atualiza a lista de projéteis
        atualizaListaProjeteis :: [Projetil] -> [Projetil]
        atualizaListaProjeteis [] = []
        atualizaListaProjeteis (p:ps)
            | inimigoEstaNoAlcance = atualizarProjeteisInimigo ps p
            | otherwise = ps 
    in inimigo {
        posicaoInimigo = novaPosicao,
        direcaoInimigo = novaDirecao,
        projeteisInimigo = atualizaListaProjeteis proj
    }

{-|
Calcula a nova direção para o inimigo
-}
calculaProximoBloco :: Mapa -> Posicao -> Direcao -> Direcao
calculaProximoBloco mapa (x, y) direcao =
    let novaPosicao = proximaPosicao (x, y) direcao
        posicaoInteira = (floor (fst novaPosicao), floor (snd novaPosicao))
    in case acharTerreno mapa posicaoInteira of
        Just Terra -> direcao
        _          -> escolheNovaDirecao mapa (x, y) direcao

{-|
Determina se um inimigo alcançou a base
-}
chegouBase :: Posicao -> Inimigo -> Bool
chegouBase basePos inimigo = encontraBase basePos (posicaoInimigo inimigo)

{-|
Determina o dano que um inimigo causa à base
-}
danoSeNaBase :: Posicao -> Inimigo -> Float
danoSeNaBase basePos inimigo
  | chegouBase basePos inimigo = ataqueInimigo inimigo
  | otherwise = 0

{-|
Verifica se duas posições coincidem
-}
encontraBase :: Posicao -> Posicao -> Bool
encontraBase (x1, y1) (x2, y2) =
    floor x1 == floor x2 && floor y1 == floor y2

{-|
Escolhe uma nova direção para o inimigo com base no mapa e sua posição atual
-}
escolheNovaDirecao :: Mapa -> Posicao -> Direcao -> Direcao
escolheNovaDirecao mapa (x, y) dirAtual =
    let
        direcoesPossiveis = direcoesValidas dirAtual
        blocoAtual = (floor x, floor y)

        {-|
        Verifica se é possível mover para um bloco adjacente em uma direção específica dentro de um mapa
        -}
        testarDirecao :: Direcao -> Bool
        testarDirecao dir =
            let (dx, dy) = case dir of
                    Norte -> (0, -1)
                    Sul   -> (0, 1)
                    Este  -> (1, 0)
                    Oeste -> (-1, 0)
                proximoBloco = (fst blocoAtual + dx, snd blocoAtual + dy)
            in case acharTerreno mapa proximoBloco of
                Just Terra -> True
                _         -> False

        direcoesValidas' = filter testarDirecao direcoesPossiveis
    in case direcoesValidas' of
        (d:_) -> d
        []    -> dirAtual

{-|
Obtém o tipo de terreno de uma posição no mapa, retornando `Nothing` se estiver fora dos limites
-}
acharTerreno :: Mapa -> (Int, Int) -> Maybe Terreno
acharTerreno mapa (x, y)
  | x >= 0 && y >= 0 && y < length mapa && x < length (head mapa) = Just ((mapa !! y) !! x)
  | otherwise = Nothing

{-|
Calcula a próxima posição com base na direção
-}
proximaPosicao :: Posicao -> Direcao -> Posicao
proximaPosicao (x, y) Norte = (x, y - 1)
proximaPosicao (x, y) Sul   = (x, y + 1)
proximaPosicao (x, y) Este  = (x + 1, y)
proximaPosicao (x, y) Oeste = (x - 1, y)

{-|
Retorna as direções válidas, excluindo a direção oposta
-}
direcoesValidas :: Direcao -> [Direcao]
direcoesValidas dir = case dir of
    Norte -> [Este, Oeste] 
    Sul   -> [Este, Oeste]  
    Este  -> [Norte, Sul]   
    Oeste -> [Norte, Sul]  

{-|
Lança novos Inimigos dos Portais
-}
atualizaPortais :: Tempo -> Jogo -> Jogo
atualizaPortais tempo jogo =
  jogo
    { portaisJogo = novosPortais,
      inimigosJogo = concat novosInimigos ++ inimigosJogo jogo }
  where
    (novosPortais, novosInimigos) = unzip $ map (lancarInimigos tempo) (portaisJogo jogo)

{-|
Função que lança inimigos a partir de um portal com base no tempo
-}
lancarInimigos :: Tempo -> Portal -> (Portal, [Inimigo])
lancarInimigos tempo portal@(Portal {ondasPortal = []}) = (portal, [])
lancarInimigos tempo portal =
    ativaInimigo tempo portal []
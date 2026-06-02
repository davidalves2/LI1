{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import ImmutableTowers
import Desenhar

{-|
Controla todos os eventos do jogo, alterando o estado de acordo com as interações do jogador
-}
reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) game@ImmutableTowers{gameState = MenuState, menu = menu}
  | dentroRetangulo (x, y) (buttonPlay menu) = game { gameState = WaitingToStart }
  | dentroRetangulo (x, y) (buttonExit menu) = error "Sair"
  | otherwise = game

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) game@ImmutableTowers{gameState = MenuState} =
    game { gameState = WaitingToStart }

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) game@ImmutableTowers{gameState = MenuState} =
    error "Sair"

reageEventos (EventKey _ Down _ _) game@ImmutableTowers{gameState = WaitingToStart} =
    game { gameState = PlayingState }

reageEventos (EventKey (Char k) Down _ _) game@ImmutableTowers{gameState = PlayingState} =
    case k of
        '1' -> game { torreSelecionada = TorreFogo }
        '2' -> game { torreSelecionada = TorreGelo }
        '3' -> game { torreSelecionada = TorreResina }
        _   -> game

reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) game@ImmutableTowers{
    gameState = PlayingState,
    torreSelecionada = torreSel,
    initialState = jogo@(Jogo {baseJogo = base}),
    mapaFormatado = mapa,
    loja = loja
} =
    let posicaoClique = converterCoordenadasParaPosicao (x, y)
        creditos = creditosBase base
    in case torreSel of
           NenhumaTorre -> game
           _ ->
               if podeColocarTorre posicaoClique mapa creditos torreSel loja jogo 
               then colocarTorre game posicaoClique
               else game

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) game@ImmutableTowers{gameState = WinningState} =
    resetJogo game

reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) game@ImmutableTowers{gameState = LoseState} =
    resetJogo game

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) game@ImmutableTowers{gameState = WinningState} =
    error "Sair"

reageEventos (EventKey (SpecialKey KeyEsc) Down _ _) game@ImmutableTowers{gameState = LoseState} =
    error "Sair"

reageEventos _ game = game


{-|
Converte coordenadas de tela para posições no mapa isométrico
-}
converterCoordenadasParaPosicao :: (Float, Float) -> (Int, Int)
converterCoordenadasParaPosicao (screenX, screenY) =
    let
        tileWidth = 62 * offsetmult
        tileHeight = 32 * offsetmult

        -- Ajustamos as coordenadas da tela
        adjustedY = -(screenY - 90 * (offsetmult + 0.5))

        tw2 = tileWidth / 2
        th2 = tileHeight / 2

        -- Calculamos as coordenadas base
        baseX = (screenX/tw2 + adjustedY/th2) / 2
        baseY = (adjustedY/th2 - screenX/tw2) / 2
        
        x = round baseX
        y = round baseY

    in (x, y)

{-|
Retorna o tipo de terreno em uma posição específica no mapa
-}
verificaTerreno :: (Int, Int) -> [[(Terreno, (Int, Int))]] -> Terreno
verificaTerreno (x, y) mapa =
    if y >= 0 && y < length mapa
    then case drop x (mapa !! y) of
            ((terreno, _):_) -> terreno
            [] -> Terra
    else Terra

{-|
Verifica se uma torre pode ser colocada em uma determinada posição
-}
podeColocarTorre :: (Int, Int) -> [[(Terreno, (Int, Int))]] -> Int -> TorreSelecionada -> Loja -> Jogo -> Bool
podeColocarTorre pos mapa creditos torreSel loja jogo =
    let existeTorre = any (\torre -> posicaoTorre torre == (fromIntegral (fst pos), fromIntegral (snd pos))) (torresJogo jogo)
        temRelvaECreditos = verificaTerreno pos mapa == Relva && creditos >= getCustoTorre torreSel loja
    in temRelvaECreditos && not existeTorre

{-|
Retorna o custo de uma torre com base no tipo selecionado
-}
getCustoTorre :: TorreSelecionada -> Loja -> Int
getCustoTorre torreSel _ = case torreSel of
    TorreFogo -> 50
    TorreGelo -> 100
    TorreResina -> 150
    NenhumaTorre -> 0

{-|
Atualiza o estado do jogo ao adicionar uma torre
-}
colocarTorre :: ImmutableTowers -> (Int, Int) -> ImmutableTowers
colocarTorre game pos =
    let jogo = initialState game
        base = baseJogo jogo
        custoTorre = getCustoTorre (torreSelecionada game) (loja game)
        novaTorre = criarTorre (torreSelecionada game) (convertPos pos)
        novoCreditos = creditosBase base - custoTorre
        novaBase = base { creditosBase = novoCreditos }
        novoJogo = jogo {
            torresJogo = novaTorre : torresJogo jogo,
            baseJogo = novaBase
        }
    in game {
           initialState = novoJogo,
           torreSelecionada = NenhumaTorre
       }

{-|
Converte coordenadas inteiras para ponto flutuante
-}
convertPos :: (Int, Int) -> (Float, Float)
convertPos (x, y) = (fromIntegral x, fromIntegral y)

{-|
Gera uma instância de Torre com base no tipo e na posição fornecidos
-}
criarTorre :: TorreSelecionada -> (Float, Float) -> Torre
criarTorre tipo pos = case tipo of
    TorreFogo -> Torre {
        posicaoTorre = pos,
        projetilTorre = Projetil Fogo (Finita 3),
        cicloTorre = 5,
        tempoTorre = 5,
        alcanceTorre = 2.5,
        rajadaTorre = 2,
        danoTorre = 0.9
    }
    TorreGelo -> Torre {
        posicaoTorre = pos,
        projetilTorre = Projetil Gelo (Finita 1),
        cicloTorre = 10,
        tempoTorre = 10,
        alcanceTorre = 2,
        rajadaTorre = 2,
        danoTorre = 0.7
    }
    TorreResina -> Torre {
        posicaoTorre = pos,
        projetilTorre = Projetil Resina (Finita 3),
        cicloTorre = 10,
        tempoTorre = 10,
        alcanceTorre = 2.5,
        rajadaTorre = 2,
        danoTorre = 0.5
    }
    NenhumaTorre -> error "Tentativa de criar torre sem tipo selecionado"

{-|
Verifica se um ponto está dentro de um retângulo
-}
dentroRetangulo :: (Float, Float) -> (Float, Float, Float, Float) -> Bool
dentroRetangulo (px, py) (x, y, w, h) =
    px >= x && px <= x + w && py >= y && py <= y + h
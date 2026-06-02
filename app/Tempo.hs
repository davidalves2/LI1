module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa2

{-|
É responsável por atualizar o estado do jogo com base no tempo decorrido e no estado atual do jogo 
-}
reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo tempo immutableTowers@ImmutableTowers { gameState = state, initialState = jogo, mapaFormatado = mapa, images = img, menu = menu } =
   case state of
       PlayingState ->
           let jogoAtualizado = atualizaJogo tempo jogo
               novoEstado
                 | ganhouJogo jogoAtualizado = WinningState
                 | perdeuJogo jogoAtualizado = LoseState
                 | otherwise = state
           in immutableTowers {
               gameState = novoEstado,
               initialState = jogoAtualizado,
               mapaFormatado = mapa,
               images = img,
               menu = menu
           }
       _ -> immutableTowers
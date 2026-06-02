{-# OPTIONS_GHC -Wno-type-defaults #-}
module ImmutableTowers where

import LI12425
import Graphics.Gloss

type Imagens = [(String,Picture)]

{-|
Mapa do jogo
-}
mapa01 :: Mapa
mapa01 =
    [ [t, t, t, t, t, t, t, t, t],
    [r, r, r, r, r, a, r, r, t],
    [r, r, r, r, r, a, r, r, t],
    [a, t, t, t, t, t, t, t, t],
    [a, t, r, r, r, a, r, r, r],
    [a, t, r, r, r, a, a, a, a],
    [a, t, t, t, t, t, t, t, a],
    [a, r, r, r, r, r, r, t, a],
    [a, t, t, t, t, t, t, t, a]
    ] where
            t = Terra
            r = Relva
            a = Agua

data GameState = MenuState | WaitingToStart | PlayingState | WinningState | LoseState
  deriving Eq

data TorreSelecionada = NenhumaTorre | TorreFogo | TorreGelo | TorreResina
  deriving Eq

data Menu = Menu {
  backgroundImage :: Picture,
  buttonPlay :: (Float, Float, Float, Float),
  buttonExit :: (Float, Float, Float, Float)
}

data ImmutableTowers = ImmutableTowers {
  gameState :: GameState,
  menu :: Menu,
  initialState :: Jogo,
  mapaFormatado :: [[(Terreno, (Int, Int))]],
  images :: Imagens,
  loja :: Loja,
  torreSelecionada :: TorreSelecionada
}

{-|
Função que faz com que o jogo volte ao seu estado Inicial
-}
resetJogo :: ImmutableTowers -> ImmutableTowers
resetJogo game = game {
    initialState = estadoInicial,
    gameState = MenuState,
    torreSelecionada = NenhumaTorre
}

{-|
Estado Inicial do jogo
-}
estadoInicial :: Jogo
estadoInicial = Jogo
    (Base {posicaoBase = (1.5, 8.5), creditosBase = 200, vidaBase = 1000})
    [ Portal { posicaoPortal = (0, 0),
               ondasPortal =
                 [ Onda { entradaOnda = 0, cicloOnda = 5, tempoOnda = 0,
                          inimigosOnda =
                            [ Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 0.5, velocidadeInimigo = 5,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Sul,
                                        butimInimigo = 25, ataqueInimigo = 0.5, velocidadeInimigo = 5,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Sul,
                                        butimInimigo = 25, ataqueInimigo = 0.5, velocidadeInimigo = 5,
                                        vidaInimigo = 100, projeteisInimigo = [] }
                            ]
                        },
                   Onda { entradaOnda = 15, cicloOnda = 4, tempoOnda = 15,
                          inimigosOnda =
                            [ Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 0.75, velocidadeInimigo = 6,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 0.75, velocidadeInimigo = 6,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 0.75, velocidadeInimigo = 6,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 0.75, velocidadeInimigo = 6,
                                        vidaInimigo = 100, projeteisInimigo = [] }
                            ]
                        },
                   Onda { entradaOnda = 25, cicloOnda = 3, tempoOnda = 15,
                          inimigosOnda =
                            [ Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 1, velocidadeInimigo = 7,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 1, velocidadeInimigo = 7,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 1, velocidadeInimigo = 7,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 1, velocidadeInimigo = 7,
                                        vidaInimigo = 100, projeteisInimigo = [] },
                              Inimigo { posicaoInimigo = (0, 0), direcaoInimigo = Este,
                                        butimInimigo = 25, ataqueInimigo = 1, velocidadeInimigo = 7,
                                        vidaInimigo = 100, projeteisInimigo = [] }
                            ]
                        }
                 ]
             }
    ]
    []
    mapa01
    []
    lojaTorre

{-|
Função que define a loja do jogo
-}
lojaTorre :: Loja
lojaTorre = [
    (50, Torre { posicaoTorre = (0,0), projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Finita 3 }, 
                 cicloTorre = 5, tempoTorre = 5, alcanceTorre = 2.5, rajadaTorre = 2, danoTorre = 1 }),
    (100, Torre { posicaoTorre = (0,0), projetilTorre = Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 1 }, 
                  cicloTorre = 10, tempoTorre = 10, alcanceTorre = 2, rajadaTorre = 2, danoTorre = 0.5 }),
    (150, Torre { posicaoTorre = (0,0), projetilTorre = Projetil { tipoProjetil = Resina, duracaoProjetil = Finita 3 }, 
                  cicloTorre = 6, tempoTorre = 2, alcanceTorre = 2.5, rajadaTorre = 2, danoTorre = 0.5 })
    ]
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Tarefa3Spec (testesTarefa3) where
import Test.HUnit 
import Tarefa3
import LI12425

estadoInicial :: Jogo
estadoInicial = Jogo {
    torresJogo = [Torre {tempoTorre = 2, rajadaTorre = 1, posicaoTorre = (0, 0), danoTorre = 1}],
    inimigosJogo = [Inimigo {posicaoInimigo = (1.5, 1.5), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Sul, projeteisInimigo = []}],
    baseJogo = Base {posicaoBase = (0.5, 0.5), vidaBase = 100, creditosBase = 0},
    portaisJogo = [],
    mapaJogo = [[Terra,Terra],
               [Agua,Terra]]
}

testAtualizaTempoTorre :: Test
testAtualizaTempoTorre = TestList
  [ "O tempo da torre deve ser decrementado" ~: 
      let torre = Torre {tempoTorre = 10, rajadaTorre = 1, posicaoTorre = (0, 0)}
          torreAtualizada = atualizaTempoTorre 5 torre
      in tempoTorre torreAtualizada ~?= 5
  ]

testAtualizaJogo :: Test
testAtualizaJogo = TestList
  [ "As torres devem ser atualizadas" ~: 
      let jogoAtualizado = atualizaJogo 1 estadoInicial
      in tempoTorre (head (torresJogo jogoAtualizado)) ~?= 1
  , "O inimigo ainda deve estar vivo" ~:
      let jogoAtualizado = atualizaJogo 1 estadoInicial
      in vidaInimigo (head (inimigosJogo jogoAtualizado)) ~?= 10
  ]

testAtualizaInimigos :: Test
testAtualizaInimigos = TestList
  [ "O inimigo deve ter se movido" ~:
      let jogoComInimigosAtualizados = atualizaInimigos 1 estadoInicial
          inimigoAtualizado = head (inimigosJogo jogoComInimigosAtualizados)
          novaPosicao = posicaoInimigo inimigoAtualizado
      in novaPosicao ~?= (1.5, 1.502)
  ]

testDisparaTorre :: Test
testDisparaTorre = TestList
  [ "O inimigo deve estar com menos vida após ser atingido" ~:
      let torre = Torre {tempoTorre = 0, rajadaTorre = 1, posicaoTorre = (0, 0), alcanceTorre = 5, danoTorre = 10}
          inimigoInicial = Inimigo {posicaoInimigo = (0, 1), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = []}
          inimigosRestantes = disparaTorre torre [inimigoInicial]
          inimigoFinal = head inimigosRestantes
      in vidaInimigo inimigoFinal ~?= 9
  ]

testAtualizaCreditos :: Test
testAtualizaCreditos = TestList
  [ "Os créditos da base devem ser atualizados com o butim do inimigo" ~:
      let inimigoDerrotado = Inimigo {posicaoInimigo = (0, 0), vidaInimigo = 0, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = [], butimInimigo = 10}
          baseInicial = Base {posicaoBase = (0, 0), vidaBase = 100, creditosBase = 0}
          baseAtualizada = atualizaCreditos [inimigoDerrotado] baseInicial
      in creditosBase baseAtualizada ~?= 10
  ]

testChegouBase :: Test
testChegouBase = TestList
  [ "O inimigo chegou à base" ~:
      let inimigo = Inimigo {posicaoInimigo = (0, 0), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = []}
          basePos = (0, 0)
      in chegouBase basePos inimigo ~?= True
  ]

testDanoSeNaBase :: Test
testDanoSeNaBase = TestList
  [ "Dano quando o inimigo chega na base" ~:
      let basePos = (0, 0)
          inimigoChegando = Inimigo {posicaoInimigo = (0, 0), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = [], ataqueInimigo = 5}
          danoChegando = danoSeNaBase basePos inimigoChegando
      in danoChegando ~?= 5
  , "Dano quando o inimigo não chega na base" ~:
      let basePos = (0, 0)
          inimigoForaDeAlcance = Inimigo {posicaoInimigo = (1, 0), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = [], ataqueInimigo = 5}
          danoForaDeAlcance = danoSeNaBase basePos inimigoForaDeAlcance
      in danoForaDeAlcance ~?= 0
  ]

testLancarInimigos :: Test
testLancarInimigos = TestList
  [ "Deve lançar os inimigos na hora correta" ~:
      let tempo = 5
          portal = Portal {ondasPortal = [Onda {tempoOnda = 3, entradaOnda = 5, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = []}]}]}
          (_, inimigosLançados) = lancarInimigos tempo portal
      in length inimigosLançados ~?= 1
  , "O inimigo deve ter a vida inicial de 10" ~:
      let tempo = 5
          portal = Portal {ondasPortal = [Onda {tempoOnda = 3, entradaOnda = 5, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = []}]}]}
          (_, inimigosLançados) = lancarInimigos tempo portal
      in vidaInimigo (head inimigosLançados) ~?= 10
  , "A posição inicial do inimigo deve ser (1, 1)" ~:
      let tempo = 5
          portal = Portal {ondasPortal = [Onda {tempoOnda = 3, entradaOnda = 5, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 10, velocidadeInimigo = 1, direcaoInimigo = Norte, projeteisInimigo = []}]}]}
          (_, inimigosLançados) = lancarInimigos tempo portal
      in posicaoInimigo (head inimigosLançados) ~?= (1, 1)
  ]

testesTarefa3 :: Test
testesTarefa3 = TestList
  [ TestLabel "Atualiza Tempo da Torre" testAtualizaTempoTorre
  , TestLabel "Atualiza Jogo" testAtualizaJogo
  , TestLabel "Atualiza Inimigos" testAtualizaInimigos
  , TestLabel "Dispara Torre" testDisparaTorre
  , TestLabel "Atualiza Créditos" testAtualizaCreditos
  , TestLabel "Chegou na Base" testChegouBase
  , TestLabel "Dano na Base" testDanoSeNaBase
  , TestLabel "Lançou Inimigos" testLancarInimigos
  ]
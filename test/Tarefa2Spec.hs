module Tarefa2Spec (testesTarefa2) where
import Test.HUnit
import Tarefa2
import LI12425

torreTeste :: Torre
torreTeste = Torre (2, 2) 10 5 3 2 1 (Projetil Fogo (Finita 2))

inimigosTeste :: [Inimigo]
inimigosTeste =
  [ Inimigo (1, 1) Norte 50 1 10 5 []
  , Inimigo (10, 10) Norte 50 1 10 5 []
  , Inimigo (3, 3) Norte 0 1 10 5 [] -- Vida zero
  ]

portalTeste :: Portal
portalTeste = Portal (0, 0) [Onda [inimigoTeste] 0 10 5 ]

inimigoTeste :: Inimigo
inimigoTeste = Inimigo (0.5, 0.5) Norte 50 1 10 5 []

baseTeste :: Base
baseTeste = Base 100 (2.5, 2.5) 10

jogoTeste :: Jogo
jogoTeste = Jogo baseTeste [portalTeste] [torreTeste] [[Relva]] inimigosTeste [(5, torreTeste)]

testInimigosNoAlcance :: Test
testInimigosNoAlcance = TestList
  [ "Inimigos no alcance" ~: inimigosNoAlcance torreTeste inimigosTeste ~?= [head inimigosTeste]
  , "Nenhum inimigo no alcance" ~: inimigosNoAlcance torreTeste [last inimigosTeste] ~?= []
  ]

testDistancia :: Test
testDistancia = TestList
  [ "Distância simples" ~: distancia (0, 0) (3, 4) ~?= 5
  , "Distância zero" ~: distancia (1, 1) (1, 1) ~?= 0
  ]

testAtingeInimigo :: Test
testAtingeInimigo = TestList
  [ "Reduz vida do inimigo" ~:
      atingeInimigo torreTeste (head inimigosTeste) ~?= (head inimigosTeste) {vidaInimigo = 49, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]}
  , "Projétil adicionado ao inimigo" ~:
      projeteisInimigo (atingeInimigo torreTeste (head inimigosTeste)) ~?= [projetilTorre torreTeste]
  ]

testAtualizarProjeteisInimigo :: Test
testAtualizarProjeteisInimigo = TestList
  [ "Adiciona projétil" ~:
      atualizarProjeteisInimigo [] (Projetil Fogo (Finita 2)) ~?= [Projetil Fogo (Finita 2)]
  , "Soma duração de projétil existente" ~:
      atualizarProjeteisInimigo [Projetil Fogo (Finita 2)] (Projetil Fogo (Finita 3)) ~?= [Projetil Fogo (Finita 5)]
  , "Remove Fogo e Gelo ao mesmo tempo" ~:
      atualizarProjeteisInimigo [Projetil Fogo (Finita 2), Projetil Gelo (Finita 3)] (Projetil Gelo (Finita 1)) ~?= []
  , "Dobra duração do Fogo e remove Resina" ~:
      atualizarProjeteisInimigo [Projetil Fogo (Finita 2), Projetil Resina (Finita 4)] (Projetil Resina (Finita 1)) 
      ~?= [Projetil Fogo (Finita 4)]  
  ]

testAtivaInimigo :: Test
testAtivaInimigo = TestList
  [ "Ativa inimigo do portal" ~:
      let (novoPortal, novosInimigos) = ativaInimigo 5 portalTeste []
      in TestList
        [ "Um inimigo ativado" ~: length novosInimigos ~?= 1
        , "Onda esvaziada" ~: null (inimigosOnda (head (ondasPortal novoPortal))) ~?= True
        ]
  , "Não ativa inimigo antes do tempo" ~:
      let (novoPortal, novosInimigos) = ativaInimigo 1 portalTeste []
      in TestList
        [ "Nenhum inimigo ativado" ~: length novosInimigos ~?= 0
        , "Tempo de entrada atualizado" ~: entradaOnda (head (ondasPortal novoPortal)) ~?= 4
        ]
  ]

testTerminouJogo :: Test
testTerminouJogo = TestList
  [ "Jogo ganho quando não há inimigos, a base tem vida positiva e as ondas estão vazias" ~:
      ganhouJogo jogoTeste {inimigosJogo = [], baseJogo = Base 10 (0.5, 0.5) 10, portaisJogo = [Portal (0, 0) [Onda [] 0 0 0]]} ~?= True
  , "Jogo não ganho quando a base não tem vida positiva" ~:
      ganhouJogo jogoTeste {inimigosJogo = [], baseJogo = Base 0 (0.5, 0.5) 10, portaisJogo = [Portal (0, 0) [Onda [] 0 0 0]]} ~?= False
  , "Jogo não ganho quando ainda há inimigos ou ondas não estão vazias" ~:
      ganhouJogo jogoTeste {inimigosJogo = [head inimigosTeste], baseJogo = Base 10 (0.5, 0.5) 10, portaisJogo = [Portal (0,0) [Onda [head inimigosTeste] 0 0 0]]} ~?= False
  , "Jogo perdido" ~: perdeuJogo jogoTeste {baseJogo = baseTeste {vidaBase = 0}} ~?= True
  ]

testesTarefa2 :: Test
testesTarefa2 = TestList
  [ TestLabel "Inimigos no alcance" testInimigosNoAlcance
  , TestLabel "Distância" testDistancia
  , TestLabel "Atinge Inimigo" testAtingeInimigo
  , TestLabel "Atualizar Projéteis do Inimigo" testAtualizarProjeteisInimigo
  , TestLabel "Ativa Inimigo" testAtivaInimigo
  , TestLabel "Terminou Jogo" testTerminouJogo
  ]

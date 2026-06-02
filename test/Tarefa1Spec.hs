module Tarefa1Spec (testesTarefa1) where
import LI12425
import Test.HUnit
import Tarefa1

terrenoExemplo :: Mapa
terrenoExemplo =
    [ [Terra, Relva, Agua]
    , [Terra, Terra, Relva]
    , [Agua, Relva, Terra]
    ]

torreExemplo :: Torre
torreExemplo = Torre (1.5, 1.5) 10 5 3 2 1 (Projetil Fogo (Finita 2))

baseExemplo :: Base
baseExemplo = Base 100 (0.5, 0.5) 10

portalExemplo :: Portal
portalExemplo = Portal (0, 0) [Onda [inimigoExemplo] 2 1 0]

inimigoExemplo :: Inimigo
inimigoExemplo = Inimigo (0.5, 0.5) Norte 50 1 10 5 []

ondaExemplo1 :: Onda
ondaExemplo1 = Onda { entradaOnda = 0, cicloOnda = 1, tempoOnda = 0, inimigosOnda = [inimigoExemplo] }

ondaExemplo2 :: Onda
ondaExemplo2 = Onda { entradaOnda = 0, cicloOnda = 2, tempoOnda = 0, inimigosOnda = [inimigoExemplo, inimigoExemplo] }

portalExemploAtivo :: Portal
portalExemploAtivo = Portal { posicaoPortal = (0, 0), ondasPortal = [ondaExemplo1] }

portalExemploDuasOndas :: Portal
portalExemploDuasOndas = Portal { posicaoPortal = (0, 0), ondasPortal = [ondaExemplo2, ondaExemplo1] }

testValidaBaseSobreTerraCredito :: Test
testValidaBaseSobreTerraCredito = TestList
    [ "Base sobre Terra" ~: validaBaseSobreTerraCredito terrenoTransformado baseExemplo ~?= True
    , "Base com créditos negativos" ~: validaBaseSobreTerraCredito terrenoTransformado (Base 100 (0.5, 0.5) (-5)) ~?= False
    , "Base não sobre Terra" ~: validaBaseSobreTerraCredito terrenoTransformado (Base 100 (1.5,0.5) 10) ~?= False
    ]
  where
    terrenoTransformado = transformarMapa terrenoExemplo 0

testValidaCicloTorre :: Test
testValidaCicloTorre = TestList
    [ "Ciclo válido" ~: validaCicloTorre [torreExemplo] ~?= True
    , "Ciclo inválido" ~: validaCicloTorre [torreExemplo {cicloTorre = -1}] ~?= False
    ]

testValidaSobreposicaoTorres :: Test
testValidaSobreposicaoTorres = TestList
    [ "Sem sobreposição" ~: validaSobreposicaoTorres [torreExemplo] ~?= True
    , "Com sobreposição" ~: validaSobreposicaoTorres [torreExemplo, torreExemplo {posicaoTorre = (1.5, 1.5)}] ~?= False
    ]

testValidaProjeteis :: Test
testValidaProjeteis = TestList
    [ "Projéteis válidos" ~: validaProjeteis [Projetil Fogo (Finita 2)] ~?= True
    , "Projéteis duplicados" ~: validaProjeteis [Projetil Fogo (Finita 2), Projetil Fogo (Infinita)] ~?= False
    , "Combinação inválida (Fogo e Gelo)" ~: validaProjeteis [Projetil Fogo (Finita 2), Projetil Gelo (Finita 2)] ~?= False
    ]

testValidaInimigoSobreTerra :: Test
testValidaInimigoSobreTerra = TestList
    [ "Inimigo sobre Terra" ~: inimigoSobreTerra terrenoTransformado [inimigoExemplo] ~?= True
    , "Inimigo não sobre Terra" ~: inimigoSobreTerra terrenoTransformado [inimigoExemplo {posicaoInimigo = (1.5, 2.5)}] ~?= False
    ]
  where
    terrenoTransformado = transformarMapa terrenoExemplo 0

testValidaPortais :: Test
testValidaPortais = TestList
    [ "Portal válido" ~: validaPortais [portalExemplo] terrenoExemplo baseExemplo [torreExemplo] ~?= True
    , "Portal sobre Relva" ~: validaPortais [portalExemplo {posicaoPortal = (1.5, 0.5)}] terrenoExemplo baseExemplo [torreExemplo] ~?= False
    , "Portal sobreposto à Base" ~: validaPortais [portalExemplo {posicaoPortal = (0.5, 0.5)}] terrenoExemplo baseExemplo [torreExemplo] ~?= False
    ]

testValidaSobreposicaoBase :: Test
testValidaSobreposicaoBase = TestList
    [ "Base válida não sobreposta" ~: 
        validaSobreposicaoBase [torreExemplo] [portalExemplo] baseExemplo ~?= True
    , "Base inválida sobreposta com torre" ~: 
        validaSobreposicaoBase [torreExemplo] [portalExemplo] baseExemplo { posicaoBase = (1.5, 1.5) } ~?= False
    ]

testValidaSobreRelvaAlcanceRajada :: Test
testValidaSobreRelvaAlcanceRajada = TestList
    [ "Torre válida sobre relva e com valores positivos" ~: 
        validaSobreRelvaAlcanceRajada terrenoTransformado [torreExemplo { posicaoTorre = (1.5, 0) }] ~?= True
    , "Torre inválida fora de relva" ~: 
        validaSobreRelvaAlcanceRajada terrenoTransformado [torreExemplo { posicaoTorre = (0, 0) }] ~?= False
    , "Torre inválida com alcance negativo" ~: 
        validaSobreRelvaAlcanceRajada terrenoTransformado [torreExemplo { alcanceTorre = -1 }] ~?= False
    ]
  where
    terrenoTransformado = transformarMapa terrenoExemplo 0

testValidaPosicaoPortal :: Test
testValidaPosicaoPortal = TestList
    [ "Portal com inimigo na posição correta" ~: 
        validaPosicaoPortal [portalExemplo] ~?= True
    , "Portal com inimigo fora da posição do portal" ~: 
        validaPosicaoPortal [portalExemplo { ondasPortal = [Onda [inimigoExemplo { posicaoInimigo = (1, 1) }] 2 1 0] }] ~?= False
    , "Portal com inimigo de vida negativa" ~: 
        validaPosicaoPortal [portalExemplo { ondasPortal = [Onda [inimigoExemplo { vidaInimigo = -10 }] 2 1 0] }] ~?= False
    ]

testExisteCaminho :: Test
testExisteCaminho = TestList
    [ "Caminho existente entre portal e base" ~: 
        existeCaminho baseExemplo [portalExemplo] terrenoExemplo ~?= True
    , "Sem caminho entre portal e base" ~: 
        existeCaminho baseExemplo [portalExemplo { posicaoPortal = (2, 2) }] terrenoExemplo ~?= False
    ]

testOndaAtivaPorPortal :: Test
testOndaAtivaPorPortal = TestList
    [ "O portal tem uma onda ativa" ~: ondaAtivaPorPortal portalExemploAtivo ~?= True
    , "O portal tem mais de uma onda ativa" ~: ondaAtivaPorPortal portalExemploDuasOndas ~?= False
    ]

testVerificaOndasAtivas :: Test
testVerificaOndasAtivas = TestList
    [ "Verifica que todos os portais têm no máximo uma onda ativa" ~: verificaOndasAtivas [portalExemploAtivo, portalExemploDuasOndas] ~?= False
    , "Verifica que um portal com uma onda ativa é válido" ~: verificaOndasAtivas [portalExemploAtivo] ~?= True
    , "Verifica que o portal com duas ondas ativas não é válido" ~: verificaOndasAtivas [portalExemploDuasOndas] ~?= False
    ]

testesTarefa1 :: Test
testesTarefa1 = TestList
    [ TestLabel "Valida Base Sobre Terra e Créditos" testValidaBaseSobreTerraCredito
    , TestLabel "Valida Ciclo Torre" testValidaCicloTorre
    , TestLabel "Valida Sobreposição Torres" testValidaSobreposicaoTorres
    , TestLabel "Valida Projéteis" testValidaProjeteis
    , TestLabel "Valida Inimigos Sobre Terra" testValidaInimigoSobreTerra
    , TestLabel "Valida Portais" testValidaPortais
    , TestLabel "Valida Sobreposição Base" testValidaSobreposicaoBase
    , TestLabel "Valida Sobre Relva e Alcance Rajada" testValidaSobreRelvaAlcanceRajada
    , TestLabel "Valida Posição Portal" testValidaPosicaoPortal
    , TestLabel "Existe Caminho" testExisteCaminho
    , TestLabel "Onda Ativa por Portal" testOndaAtivaPorPortal
    , TestLabel "Verifica Ondas Ativas" testVerificaOndasAtivas
    ]



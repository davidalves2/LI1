{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Desenhar where

import LI12425
import Graphics.Gloss

import ImmutableTowers hiding (lojaTorre)

{-|
Controla a escala dos elementos visuais do jogo [recomendado 1-2]
-}
offsetmult :: Float
offsetmult = 1.5

{-|
Função principal de renderização que decide qual estado do jogo desenhar
-}
desenha :: ImmutableTowers -> Picture
desenha game@ImmutableTowers{gameState = state} =
  case state of
    MenuState -> desenhaMenu game
    WaitingToStart -> desenhaJogo game
    PlayingState -> desenhaJogo game
    WinningState -> desenhaWin game
    LoseState -> desenhaLose game

{-|
Renderiza a tela de vitória com botões para jogar novamente ou sair
-}
desenhaWin :: ImmutableTowers -> Picture
desenhaWin ImmutableTowers{images = img} =
    pictures [
    -- Fundo
    scale 0.90 0.90 $ lookupImage "win" (img),
    translate 10 (-160) $ scale (0.30 * offsetmult) (0.30 * offsetmult)  $ lookupImage "jogardenovo" ( img),
    translate 10 (-230) $ scale (0.21 * offsetmult) (0.21 * offsetmult)  $ lookupImage "sair2" ( img)
  ]

{-|
Renderiza a tela de derrota com botões para jogar novamente ou sair
-}
desenhaLose :: ImmutableTowers -> Picture
desenhaLose ImmutableTowers{images = img} =
    pictures [
    -- Fundo
    scale 1.1 1.1 $ lookupImage "loss" ( img),
    translate 10 (-160) $ scale (0.30 * offsetmult) (0.30 * offsetmult)  $ lookupImage "jogardenovo" ( img),
    translate 10 (-230) $ scale (0.21 * offsetmult) (0.21 * offsetmult)  $ lookupImage "sair2" ( img)
  ]

{-|
Renderiza o menu inicial do jogo com opções para "Jogar" ou "Sair"
-}
desenhaMenu :: ImmutableTowers -> Picture
desenhaMenu ImmutableTowers{images = img} =
  pictures [
    -- Fundo
    scale 0.370 0.360 $ lookupImage "fundo_menu" (img),

    -- Botão Jogar
    translate (-25) (-110) $
    color (makeColor 0.2 0.2 0.2 0.8) $
    rectangleSolid 200 50,

    -- Texto "Jogar" centralizado e com letras mais grossas
    translate (-20) (-110) $ scale (0.18 * offsetmult) (0.18 * offsetmult)  $ lookupImage "jogar" ( img),


    -- Botão Sair
    translate (-25) (-210) $
    color (makeColor 0.2 0.2 0.2 0.8) $
    rectangleSolid 200 50,

    -- Texto "Sair" centralizado e com letras mais grossas
    translate (-20) (-210) $ scale (0.18 * offsetmult) (0.18 * offsetmult)  $ lookupImage "sair" ( img)
  ]

{-|
Cria um texto estilizado com bordas espessas
-}
thickText :: String -> Picture
thickText str = pictures $ map (\(dx, dy) -> translate dx dy (text str)) offsets
  where
    offsets = [(-7.3, 0), (7.3, 0), (0, -7.3), (0, 7.3)]  

{-|
Renderiza o estado principal do jogo, incluindo o terreno, torres, inimigos, base e loja
-}
desenhaJogo :: ImmutableTowers -> Picture
-- Caso para PlayingState com lista vazia
desenhaJogo ImmutableTowers {gameState = PlayingState, initialState = Jogo {baseJogo = _, portaisJogo = _, torresJogo = _}, mapaFormatado = [], images = _} = blank

-- Caso para PlayingState com lista não vazia
desenhaJogo ImmutableTowers {gameState = PlayingState, initialState = game@(Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, inimigosJogo = inimigos}), mapaFormatado = (h:t), images = img, loja = loja, torreSelecionada = torreSel} =
 pictures [desenhaBaseTerra ( img) h,
           desenhaLinhaMapa ( img) h,
           desenhaBase ( img) base,
           desenhaPortais ( img) portais,
           desenhaTorres ( img) torres inimigos PlayingState,
           desenhaInimigos ( img) inimigos,
           desenhaLoja ( img) base loja,
           desenhaMensagemTorre torreSel,
           -- Adicionado o campo torreSelecionada
           desenha ImmutableTowers {gameState = PlayingState, initialState = game, mapaFormatado = t, images = img, loja = loja, torreSelecionada = torreSel}
          ]

-- Caso para WaitingToStart com lista vazia
desenhaJogo ImmutableTowers {gameState = WaitingToStart, mapaFormatado = [], images = _} = blank

-- Caso para WaitingToStart com lista não vazia
desenhaJogo ImmutableTowers {gameState = WaitingToStart, initialState = game@(Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, inimigosJogo = inimigos}), mapaFormatado = (h:t), images = img, loja = loja, torreSelecionada = torreSel} =
 pictures [desenhaBaseTerra ( img) h,
           desenhaLinhaMapa ( img) h,
           desenhaBase ( img) base,
           desenhaPortais ( img) portais,
           desenhaTorres ( img) torres inimigos WaitingToStart,
           desenhaInimigos ( img) inimigos,
           desenha ImmutableTowers {gameState = WaitingToStart, initialState = game, mapaFormatado = t, images = img, loja = loja, torreSelecionada = torreSel},
           -- Texto adicional para WaitingToStart
           translate (-220) (-300) $ scale 0.2 0.2 $ color white $ thickText "Pressione qualquer tecla para jogar!"
          ]

-- Caso padrão para outros estados
desenhaJogo ImmutableTowers {mapaFormatado = []} = blank
desenhaJogo _ = blank

{-|
Exibe mensagens indicando a torre atualmente selecionada
-}
desenhaMensagemTorre :: TorreSelecionada -> Picture
desenhaMensagemTorre torreSel =
    let mensagem = case torreSel of
            TorreFogo -> "Torre de Fogo selecionada (50$)"
            TorreGelo -> "Torre de Gelo selecionada (100$)"
            TorreResina -> "Torre de Resina selecionada (150$)"
            NenhumaTorre -> ""
    in if mensagem /= ""
       then translate (-220) 300 $ scale 0.2 0.2 $ color white $ thickText mensagem
       else blank

{-|
Renderiza a camada de base do terreno
-}
desenhaBaseTerra :: Imagens -> [(Terreno, (Int, Int))] -> Picture
desenhaBaseTerra img linha = rotate 0 $ translate 0 (90*(offsetmult+0.38)) $ pictures (map (desenhaTerrenoTerra img) linha)

{-|
Focada em desenhar apenas os blocos do tipo "terra" no terreno
-}
desenhaTerrenoTerra :: Imagens -> (Terreno, (Int, Int)) -> Picture
desenhaTerrenoTerra img (_, (x, y)) =
  let tileWidth = 62*offsetmult
      tileHeight = 32*offsetmult
      xOffset = fromIntegral (x - y) * (tileWidth / 2)
      yOffset = fromIntegral (x + y) * (tileHeight / 2)
  in pictures [rotate 180 $ translate xOffset yOffset $ scale (0.275*offsetmult) (0.275*offsetmult) (lookupImage "terra" img)]

{-|
Renderiza uma linha do mapa com os tipos de terreno
-}
desenhaLinhaMapa :: Imagens -> [(Terreno, (Int, Int))] -> Picture
desenhaLinhaMapa img linha = rotate 0 $ translate 0 (90*(offsetmult+0.5)) $ pictures (map (desenhaTerreno img) linha)

{-|
Responsável por renderizar o terreno completo do mapa
-}
desenhaTerreno :: Imagens -> (Terreno, (Int, Int)) -> Picture
desenhaTerreno img (terreno, (x, y)) =
  let tileWidth = 62*offsetmult
      tileHeight = 32*offsetmult
      xOffset = fromIntegral (x - y) * (tileWidth / 2)
      yOffset = fromIntegral (x + y) * (tileHeight / 2)
  in case terreno of
       Relva -> rotate 180 $ translate xOffset yOffset $ scale (0.27*offsetmult) (0.27*offsetmult) (lookupImage "relva" img)
       Terra -> rotate 180 $ translate xOffset yOffset $ scale (0.27*offsetmult) (0.27*offsetmult) (lookupImage "terra" img)
       Agua  -> rotate 180 $ translate xOffset yOffset $ scale (0.27*offsetmult) (0.27*offsetmult) (lookupImage "agua" img)

{-|
Renderiza a base do jogador com uma barra de vida colorida dependendo do estado atual
-}
desenhaBase :: Imagens -> Base -> Picture
desenhaBase img base@(Base {posicaoBase = (y, x), vidaBase = vida}) =
    let tileWidth = 62 * offsetmult
        tileHeight = 32 * offsetmult
        xOffset = fromInteger (floor x - floor y) * (tileWidth / 2)
        yOffset = fromInteger (floor x + floor y) * (tileHeight / 2)
    in pictures [
         translate 0 (90 * (offsetmult + 0.55)) $
         rotate 180 $
         translate xOffset (yOffset - 25) $
         scale (0.22 * offsetmult) (0.22 * offsetmult) (lookupImage "base" img),

         -- Contorno da barra de vida
          if vida >= 0 && vida <= 1000 then
             translate 0 (90 * (offsetmult + 1.25)) $
             rotate 180 $
             translate xOffset (yOffset-10) $
             scale 0.41 0.4 $
             color white $ rectangleWire 100 15
         else blank,

         -- Barra de vida preenchida (verde, amarelo, laranja ou vermelho)
         if vida > 750 then
             translate 0 (90 * (offsetmult + 1.25)) $
             rotate 180 $
             translate xOffset (yOffset-10) $
             scale 0.04 0.4 $
             color green $ rectangleSolid vida 12
         else if vida > 500 then
             translate 0 (90 * (offsetmult + 1.25)) $
             rotate 180 $
             translate xOffset (yOffset-10) $
             scale 0.04 0.4 $
             color yellow $ rectangleSolid vida 12
         else if vida > 250 then
             translate 0 (90 * (offsetmult + 1.25)) $
             rotate 180 $
             translate xOffset (yOffset-10) $
             scale 0.04 0.4 $
             color orange $ rectangleSolid vida 12
         else if vida > 0 then
             translate 0 (90 * (offsetmult + 1.25)) $
             rotate 180 $
             translate xOffset (yOffset-10) $
             scale 0.04 0.4 $
             color red $ rectangleSolid vida 12
         else blank
       ]

{-|
Renderiza os portais no mapa
-}
desenhaPortais :: Imagens -> [Portal] -> Picture
desenhaPortais _ [] = blank
desenhaPortais img ((Portal (y,x) _):t) =
  let tileWidth = 62 * offsetmult
      tileHeight = 32 * offsetmult
      xOffset = fromInteger (floor x - floor y) * (tileWidth / 2)
      yOffset = fromInteger (floor x + floor y) * (tileHeight / 2)
  in pictures [ translate 0 (90*(offsetmult+0.6)) $
       rotate 180 $ translate xOffset (yOffset-25) $
       scale (0.17 * offsetmult) (0.4 * offsetmult) (lookupImage "portal" img),
       desenhaPortais img t
     ]

{-|
Renderiza as torres e os projéteis disparados em inimigos dentro de seu alcance
-}
desenhaTorres :: Imagens -> [Torre] -> [Inimigo] -> GameState -> Picture
desenhaTorres _ [] _ _ = blank
desenhaTorres img (torre@Torre {posicaoTorre = (yT, xT), alcanceTorre = alcance, rajadaTorre = rajada, projetilTorre = Projetil {tipoProjetil = tipo}} : t) inimigos gameState =
  let
    -- Dimensões dos tiles
    tileWidth = 62 * offsetmult
    tileHeight = 32 * offsetmult

    -- Tamanhos para cada tipo de projétil
    tamanhoFogo = 0.02
    tamanhoGelo = 0.055
    tamanhoResina = 0.06

    -- Posição da torre em coordenadas isométricas
    xOffsetT = fromInteger (floor xT - floor yT) * (tileWidth / 2)
    yOffsetT = fromInteger (floor xT + floor yT) * (tileHeight / 2)

    -- Nome da imagem da torre com base no tipo de projétil
    imageName = case tipo of
      Fogo   -> "torrefogo"
      Gelo   -> "torregelo"
      Resina -> "torreresina"

    -- Nome e tamanho da imagem do projétil com base no tipo de torre
    (projImageName, tamanhoProj) = case tipo of
      Fogo   -> ("fogo", tamanhoFogo)
      Gelo   -> ("gelo", tamanhoGelo)
      Resina -> ("resina", tamanhoResina)

    {-|
    Função que calcula a distância entre dois pontos
    -}
    distancia :: Posicao -> Posicao -> Float
    distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

    -- Filtrar inimigos dentro do alcance da torre
    inimigosDentroAlcance = filter (\inimigo -> distancia (xT, yT) (snd (posicaoInimigo inimigo), fst (posicaoInimigo inimigo)) <= alcance) inimigos

    -- Selecionar apenas o número de inimigos permitido pela rajada
    inimigosAtacados = take rajada inimigosDentroAlcance

    -- Função para desenhar os projéteis (bolas) em direção aos inimigos
    desenhaProjeteis = pictures $ map (\(Inimigo {posicaoInimigo = (yI, xI)}) ->
      let
        xOffsetI = fromInteger (floor xI - floor yI) * (tileWidth / 2)
        yOffsetI = fromInteger (floor xI + floor yI) * (tileHeight / 2)
        xProj = xOffsetT + (xOffsetI - xOffsetT) * 0.5
        yProj = yOffsetT + (yOffsetI - yOffsetT) * 0.5
      in
        translate 0 (90 * (offsetmult + 0.6)) $
        rotate 180 $
        translate xProj yProj $
        scale tamanhoProj tamanhoProj $
        rotate 45 $
        lookupImage projImageName img) inimigosAtacados

  in pictures [
       -- Desenha a torre
       translate 0 (90 * (offsetmult + 0.6)) $
       rotate 180 $
       translate xOffsetT (yOffsetT - 30) $
       scale (0.25 * offsetmult) (0.23 * offsetmult) (lookupImage imageName img),

       -- Desenha os projéteis apenas se o estado for PlayingState
       case gameState of
         PlayingState -> desenhaProjeteis
         _ -> blank,

       -- Recursivamente desenha as torres restantes
       desenhaTorres img t inimigos gameState
     ]

{-|
Renderiza os inimigos no mapa com base na direção e projéteis recebidos
-}
desenhaInimigos :: Imagens -> [Inimigo] -> Picture
desenhaInimigos _ [] = blank
desenhaInimigos img (Inimigo {posicaoInimigo = (y,x), direcaoInimigo = direcao, vidaInimigo = vida, projeteisInimigo = projeteis} : t) =
    let tileWidth = 62 * offsetmult
        tileHeight = 32 * offsetmult
        xOffset = (x - y) * (tileWidth / 2)
        yOffset = (x + y) * (tileHeight / 2)
        skin = case direcao of
              Norte -> "enemy_n"
              Sul   -> "enemy_s"
              Este -> "enemy_e"
              Oeste -> "enemy_o"
        projetil = case () of
                  _ | any (\elemento -> tipoProjetil elemento == Fogo) projeteis ->
                      if vida>0 then translate 0 (90 * (offsetmult + 1.25)) $
                        rotate 180 $
                        translate (xOffset+30) yOffset $
                        scale 0.05 0.05 $
                        color red $
                        lookupImage "efogo" img else blank
                    | any (\elemento -> tipoProjetil elemento == Gelo) projeteis ->
                      if vida>0 then translate 0 (90 * (offsetmult + 1.25)) $
                        rotate 180 $
                        translate (xOffset+30) yOffset $
                        scale 0.01 0.01 $
                        color blue $
                        lookupImage "egelo" img else blank
                    | any (\elemento -> tipoProjetil elemento == Resina) projeteis ->
                      if vida>0 then translate 0 (90 * (offsetmult + 1.25)) $
                        rotate 180 $
                        translate (xOffset+30) yOffset $
                        scale 0.04 0.04 $
                        color green $
                        lookupImage "eresina" img  else blank
                  _ -> blank
    in pictures [
         if vida>0 then translate 0 (90 * (offsetmult + 0.45)) $
              rotate 180 $
              translate xOffset (yOffset - 30) $
              scale (0.15 * offsetmult) (0.15 * offsetmult) (lookupImage skin img) else blank,
         if vida>0 && vida<100 then translate 0 (90 * (offsetmult + 1.25)) $ rotate 180 $ translate xOffset yOffset $ scale 0.41 0.4 $
            color white $ rectangleWire 100 15
          else blank,
         if vida>75 && vida<100 then translate 0 (90 * (offsetmult + 1.25)) $ rotate 180 $ translate xOffset yOffset $ scale 0.4 0.4 $
            color green $ rectangleSolid vida 12
          else blank,
          if vida>50 && vida<=75 then translate 0 (90 * (offsetmult + 1.25)) $ rotate 180 $ translate xOffset yOffset $ scale 0.4 0.4 $
            color yellow $ rectangleSolid vida 12
          else blank,
          if vida>25 && vida<=50 then translate 0 (90 * (offsetmult + 1.25)) $ rotate 180 $ translate xOffset yOffset $ scale 0.4 0.4 $
            color orange $ rectangleSolid vida 12
          else blank,
          if vida>0 && vida<=25 then translate 0 (90 * (offsetmult + 1.25)) $ rotate 180 $ translate xOffset yOffset $ scale 0.4 0.4 $
            color red $ rectangleSolid vida 12
          else blank,
          projetil,
          desenhaInimigos img t
       ]

{-|
Procura e retorna uma imagem associada a um identificador (nome) dentro da estrutura de imagens carregadas
-}
lookupImage :: String -> Imagens -> Picture
lookupImage name img = maybe blank id (lookup name img)

{-|
Responsável por renderizar a loja de torres no jogo
-}
desenhaLoja :: Imagens -> Base -> Loja -> Picture
desenhaLoja img base lojaTorre = pictures [
    -- Barra horizontal de fundo
    translate 0 (-350) $ color (makeColor 0.2 0.2 0.2 0.8) $ rectangleSolid 1300 150,

    translate (-525) (-320) $ scale (0.5 * offsetmult) (0.5 * offsetmult) (lookupImage "LOJA" img),

    -- Torre de Gelo (Primeira torre)
    translate (-135) (-370) $ rotate 180 $ scale (0.2 * offsetmult) (0.2 * offsetmult) (lookupImage "torrefogo" img),
    translate (-70) (-400) $ scale (0.05 * offsetmult) (0.05 * offsetmult) (lookupImage "50" img),
    translate (-135) (-300) $ scale (0.17 * offsetmult) (0.17 * offsetmult) (lookupImage "Tfogotext" img),

    -- Torre de Fogo (Segunda torre)
    translate 200 (-370) $ rotate 180 $ scale (0.2 * offsetmult) (0.2 * offsetmult) (lookupImage "torregelo" img),
    translate 265 (-400) $ scale (0.05 * offsetmult) (0.05 * offsetmult) (lookupImage "100" img),
    translate 200 (-300) $ scale (0.18 * offsetmult) (0.18 * offsetmult) (lookupImage "Tgelotext" img),

    -- Torre de Resina (Terceira torre)
    translate 520 (-370) $ rotate 180 $ scale (0.2 * offsetmult) (0.2 * offsetmult) (lookupImage "torreresina" img),
    translate 585 (-400) $ scale (0.05 * offsetmult) (0.05 * offsetmult) (lookupImage "150" img),
    translate 520 (-300) $ scale (0.18 * offsetmult) (0.18 * offsetmult) (lookupImage "Tresinatext" img),

    -- Mostrar créditos totais
    translate (-625) (-400) $ scale 0.2 0.2 $ color white $ thickText ("Saldo: " ++ show (creditosBase base) ++ "$")
    ]
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : José Manuel Peixoto Rocha <a106887@alunos.uminho.pt>
              David Jose Barbosa Alves <a107324@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where
import LI12425
import Data.List

{-|
Verifica se o estado de um jogo é válido
-}
validaJogo :: Jogo -> Bool
validaJogo jogo =
  validaPortais (portaisJogo jogo) (mapaJogo jogo) (baseJogo jogo) (torresJogo jogo) &&
  validaInimigos jogo (portaisJogo jogo) (inimigosJogo jogo) (torresJogo jogo) &&
  validaTorres jogo (torresJogo jogo) &&
  validaBase jogo (baseJogo jogo)

{-|
Verifica se a Base é válida
-}
validaBase :: Jogo -> Base -> Bool
validaBase jogo base =
    validaBaseSobreTerraCredito (transformarMapa (mapaJogo jogo) 0) base &&
    validaSobreposicaoBase (torresJogo jogo) (portaisJogo jogo) base

{-|
Verifica se a Base não está sobreposta a uma torre ou portal
-}
validaSobreposicaoBase :: [Torre] -> [Portal] -> Base -> Bool
validaSobreposicaoBase torres portais (Base {posicaoBase = basePos}) =
    not (any (`elem` allPositions) towerPositions)
  where
    allPositions = portalPositions ++ [basePos]
    towerPositions = map (\(Torre {posicaoTorre = pos}) -> pos) torres
    portalPositions = map (\(Portal {posicaoPortal = pos}) -> pos) portais

{-|
Verifica se a Base está colocada sobre terra e não tem crédito negativo
-}
validaBaseSobreTerraCredito :: [[(Terreno, (Int, Int))]] -> Base -> Bool
validaBaseSobreTerraCredito terreno (Base {posicaoBase = (x,y), creditosBase = creditos}) =
    case acharBloco terreno (fromInteger $ floor x,fromInteger $ floor y) of
        Just Terra -> creditos >=0
        _          -> False

{-|
Verifica se as Torres são válidas
-}
validaTorres :: Jogo -> [Torre] -> Bool
validaTorres _ [] = True
validaTorres jogo torres =
       validaSobreRelvaAlcanceRajada (transformarMapa (mapaJogo jogo) 0) torres &&
       validaSobreposicaoTorres torres &&
       validaCicloTorre torres

{-|
Verifica se todas as Torres possuem ciclos de disparo não negativos
-}
validaCicloTorre :: [Torre] -> Bool
validaCicloTorre [] = True
validaCicloTorre (Torre {cicloTorre = ciclo} : rest) =
  ciclo > 0 && validaCicloTorre rest

{-|
Verifica se as Torres não estão sobrepostas
-}
validaSobreposicaoTorres :: [Torre] -> Bool
validaSobreposicaoTorres [] = True -- Base case: no towers, no overlap
validaSobreposicaoTorres (Torre {posicaoTorre = (x, y)} : rest) =
    not (any (\(Torre {posicaoTorre = (x1, y1)}) -> (floor x1, floor y1) == (floor x, floor y)) rest) &&
    validaSobreposicaoTorres rest

{-|
Verifica se as Torres estão sobre relva e se o seu alcance é um valor positivo
-}
validaSobreRelvaAlcanceRajada :: [[(Terreno, (Int, Int))]] -> [Torre] -> Bool
validaSobreRelvaAlcanceRajada _ [] = True
validaSobreRelvaAlcanceRajada terreno (Torre {posicaoTorre = (x,y), alcanceTorre = alcance, rajadaTorre = rajada}:rest) =
        case acharBloco terreno (fromInteger $ floor x,fromInteger $ floor y) of
        Just Relva -> alcance > 0 && rajada>0 && validaSobreRelvaAlcanceRajada terreno rest
        _          -> False

{-|
Verifica se os Inimigos são válidos
-}
validaInimigos :: Jogo -> [Portal] -> [Inimigo] -> [Torre] -> Bool
validaInimigos jogo portais inimigos torres =
      validaPosicaoPortal portais &&
      inimigoSobreTerra (transformarMapa (mapaJogo jogo) 0) inimigos &&
      sobreposicaoTorre (transformarMapa (mapaJogo jogo) 0) inimigos torres &&
      all (validaProjeteis . projeteisInimigo) inimigos

{-|
Verifica se a lista de projéteis ativos de um Inimigo encontra-se normalizada
-}
validaProjeteis :: [Projetil] -> Bool
validaProjeteis projeteis =
  tiposUnicos projeteis && combinacaoValida projeteis
  where
    -- Verifica se não há mais de um projétil do mesmo tipo
    tiposUnicos :: [Projetil] -> Bool
    tiposUnicos ps =
      let tipos = map tipoProjetil ps
       in length tipos == length (nub tipos) 

    -- Verifica se não há combinações inválidas de tipos de projéteis
    combinacaoValida :: [Projetil] -> Bool
    combinacaoValida ps =
      let tipos = map tipoProjetil ps
       in not (Fogo `elem` tipos && Resina `elem` tipos) && 
          not (Fogo `elem` tipos && Gelo `elem` tipos)     

{-|
Verifica se todos os Inimigos por lançar têm a posição do respetivo portal, nível de vida positivo, e lista de projéteis ativos vazia
-}
validaPosicaoPortal :: [Portal] -> Bool
validaPosicaoPortal [] = True
validaPosicaoPortal (Portal {posicaoPortal = portalPos, ondasPortal = ondas} : rest) =
  all validaInimigosOnda ondas && validaPosicaoPortal rest
  where
    -- Valida todos os enimigos na onda
    validaInimigosOnda :: Onda -> Bool
    validaInimigosOnda (Onda {inimigosOnda = inimigos}) =
      all validaInimigo inimigos

    -- Valida um só inimigo
    validaInimigo :: Inimigo -> Bool
    validaInimigo (Inimigo {posicaoInimigo = (x,y), vidaInimigo = vida, projeteisInimigo = projeteis}) =
      (fromInteger $ floor x,fromInteger $ floor y) == portalPos && vida > 0 && null projeteis

{-|
Verifica se todos os Inimigos em jogo encontram-se sobre terra
-}
inimigoSobreTerra :: [[(Terreno, (Int, Int))]] -> [Inimigo] -> Bool
inimigoSobreTerra _ [] = True
inimigoSobreTerra terreno (Inimigo {posicaoInimigo = (x,y)} : rest) =
  case acharBloco terreno (fromInteger $ floor x,fromInteger $ floor y) of
    Just Terra -> inimigoSobreTerra terreno rest
    _          -> False 

{-|
Verifica se os Inimigos não estão sobrepostos a Torres
-}
sobreposicaoTorre :: [[(Terreno, (Int, Int))]] -> [Inimigo] -> [Torre] -> Bool
sobreposicaoTorre _ [] _ = True -- Base case: no enemies, validation passes
sobreposicaoTorre terreno (Inimigo {posicaoInimigo = (y, x), velocidadeInimigo = velocidade} : rest) torres =
  velocidade >= 0 && not (any (torreSobreposicao (y, x)) torres) && sobreposicaoTorre terreno rest torres
  where
    -- Verifica se a Torre sobrepõe-se à posição inimiga fornecida
    torreSobreposicao :: (Float, Float) -> Torre -> Bool
    torreSobreposicao (y1, x1) (Torre {posicaoTorre = (y2, x2)}) =
      floor x1 == floor x2 && floor y1 == floor y2

{-|
Verifica se os Portais são válidos
-}
validaPortais :: [Portal] -> Mapa -> Base -> [Torre] -> Bool
validaPortais [] _ _ _ = False
validaPortais portais mapa base torres =
  all (portalSobreTerra mapa) portais &&
  existeCaminho base portais mapa &&
  naoSobrepostos portais base torres &&
  verificaOndasAtivas portais

{-|
Verifica se um Portal está posicionado sobre terra
-}
portalSobreTerra :: Mapa -> Portal -> Bool
portalSobreTerra mapa (Portal (x, y) _) = case getTerreno mapa (floor x, floor y) of
  Just Terra -> True
  _ -> False

{-|
Verifica se existe um caminho de terra conectando pelo menos um Portal à Base
-}
existeCaminho :: Base -> [Portal] -> Mapa -> Bool
existeCaminho base portais mapa =
  any (\portal -> conectado (floor $ fst $ posicaoBase base) (floor $ snd $ posicaoBase base)
                           (floor $ fst $ posicaoPortal portal) (floor $ snd $ posicaoPortal portal) mapa)
      portais

{-|
Verifica se os Portais não estão sobrepostos à Base ou às Torres
-}
naoSobrepostos :: [Portal] -> Base -> [Torre] -> Bool
naoSobrepostos portais base torres =
  let posicoesProibidas = posicaoBase base : map posicaoTorre torres
   in all (\portal -> posicaoPortal portal `notElem` posicoesProibidas) portais

{-|
Verifica se há, no máximo, uma onda ativa em um portal.
-}
ondaAtivaPorPortal :: Portal -> Bool
ondaAtivaPorPortal (Portal {ondasPortal = ondas}) =
    let 
        -- Filtra ondas ativas (com entrada ou inimigos restantes)
        ondasAtivas = filter (\onda -> entradaOnda onda <= 0 ) ondas
    in 
        -- Verifica se há no máximo uma onda ativa
        length ondasAtivas <= 1

{-|
Verifica se todos os portais possuem no máximo uma onda ativa.
-}
verificaOndasAtivas :: [Portal] -> Bool
verificaOndasAtivas = all ondaAtivaPorPortal

{-|
Obtém o terreno do mapa em uma posição específica
-}
getTerreno :: Mapa -> (Int, Int) -> Maybe Terreno
getTerreno mapa (x, y)
  | x >= 0 && y >= 0 && x < length (head mapa) && y < length mapa = Just (mapa !! y !! x)
  | otherwise = Nothing

{-|
Verifica se duas posições estão conectadas no mapa.
-}
conectado :: Int -> Int -> Int -> Int -> Mapa -> Bool
conectado x1 y1 x2 y2 mapa = busca [(x1, y1)] []
  where
    busca [] _ = False
    busca (atual@(x, y):resto) visitados
      | atual == (x2, y2) = True
      | atual `elem` visitados = busca resto visitados
      | otherwise =
          let vizinhos = [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                            case getTerreno mapa (x + dx, y + dy) of
                              Just Terra -> True
                              _ -> False]
           in busca (resto ++ vizinhos) (atual : visitados)

{-|
Transforma um mapa de terrenos em um mapa de terrenos com coordenadas
-}
transformarMapa :: [[Terreno]] -> Int -> [[(Terreno, (Int, Int))]]
transformarMapa [] _ = []
transformarMapa (linha:resto) linhaIndex =
  transformarMapaL linha linhaIndex 0 : transformarMapa resto (linhaIndex + 1)

{-|
Transforma uma linha de terrenos em uma linha de terrenos com coordenadas
-}
transformarMapaL :: [Terreno] -> Int -> Int -> [(Terreno, (Int, Int))]
transformarMapaL [] _ _ = []
transformarMapaL (h:t) linhaIndex colunaIndex =
  (h, (linhaIndex, colunaIndex)) : transformarMapaL t linhaIndex (colunaIndex + 1)

{-|
Procura um terreno em uma matriz de blocos pelas coordenadas fornecidas
-}
acharBloco :: [[(Terreno, (Int, Int))]] -> (Float, Float) -> Maybe Terreno
acharBloco [] _ = Nothing
acharBloco (linha:t) (y,x) =
  case acharBlocoLinha linha (y,x) of
    Just terreno -> Just terreno
    Nothing -> acharBloco t (y,x)

{-|
Procura um terreno em uma linha de blocos pelas coordenadas fornecidas
-}
acharBlocoLinha :: [(Terreno, (Int, Int))] -> (Float, Float) -> Maybe Terreno
acharBlocoLinha [] _ = Nothing
acharBlocoLinha ((terreno, (x1, y1)):t) (y,x)
  | floor x == x1 && floor y == y1 = Just terreno
  | otherwise = acharBlocoLinha t (y,x)
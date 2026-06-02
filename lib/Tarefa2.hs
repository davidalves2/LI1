{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : José Manuel Peixoto Rocha <a106887@alunos.uminho.pt>
              David Jose Barbosa Alves <a107324@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where
import LI12425

{-|
Identifica quais Inimigos estão dentro do alcance de uma Torre 
-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre inimigos =
    [inimigo | inimigo@(Inimigo {posicaoInimigo = pos, vidaInimigo = vida}) <- inimigos,
               distancia (posicaoTorre torre) pos <= alcanceTorre torre && vida > 0]

{-|
Calcula a distância euclidiana entre duas posições
-}
distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

{-|
Função que aplica os efeitos de um ataque da Torre a um Inimigo
-}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo (Torre {danoTorre = dano, projetilTorre = proj}) (Inimigo {posicaoInimigo = pos, ataqueInimigo = ataq, butimInimigo = valor, velocidadeInimigo = vel, direcaoInimigo = dir, vidaInimigo = vida, projeteisInimigo = lista}) =
    (Inimigo { posicaoInimigo = pos,
               velocidadeInimigo = vel,
               ataqueInimigo = ataq,
               butimInimigo = valor,
               direcaoInimigo = dir,
               vidaInimigo = vida - dano / 10,
               projeteisInimigo = atualizarProjeteisInimigo lista proj })

{-|
Função para atualizar a lista de projéteis do inimigo
-}
atualizarProjeteisInimigo :: [Projetil] -> Projetil -> [Projetil]
atualizarProjeteisInimigo [] proj = [proj]
atualizarProjeteisInimigo lista proj@(Projetil {tipoProjetil = tipo}) =
    case tipo of
        Fogo   -> processaFogo lista proj
        Gelo   -> processaGelo lista proj
        Resina -> processaResina lista proj
  where
    {-|
    Remove Fogo e Gelo se ambos existirem
    -}
    processaFogo listaProj projetil =
        if Gelo `elem` map tipoProjetil listaProj
        then filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) listaProj
        else somaOuAdiciona listaProj projetil

    {-|
    Soma durações para Gelo ou cancela com Fogo
    -}
    processaGelo listaProj projetil =
        if Fogo `elem` map tipoProjetil listaProj
        then filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) listaProj
        else somaOuAdiciona listaProj projetil

    {-|
    Duração do Fogo dobra se Resina existir, remove Resina
    -}
    processaResina listaProj projetil =
        if Fogo `elem` map tipoProjetil listaProj
        then map (\p -> if tipoProjetil p == Fogo 
                        then p {duracaoProjetil = dobraDuracao (duracaoProjetil p)} 
                        else p) 
                  (filter (\p -> tipoProjetil p /= Resina) listaProj)
        else somaOuAdiciona listaProj projetil

    {-|
    Adiciona durações para o mesmo tipo ou adicione o projétil
    -}
    somaOuAdiciona :: [Projetil] -> Projetil -> [Projetil]
    somaOuAdiciona [] x = [x]
    somaOuAdiciona (p:ps) projetil
        | tipoProjetil p == tipoProjetil projetil =
            p {duracaoProjetil = somaDuracao (duracaoProjetil p) (duracaoProjetil projetil)} : ps
        | otherwise = p : somaOuAdiciona ps projetil

{-|
Função que soma as durações de projéteis
-}
somaDuracao :: Duracao -> Duracao -> Duracao
somaDuracao (Finita t1) (Finita t2) = Finita (t1 + t2)
somaDuracao _ Infinita = Infinita
somaDuracao Infinita _ = Infinita

{-|
Função que duplica a duração de um projétil
-}
dobraDuracao :: Duracao -> Duracao
dobraDuracao (Finita t) = Finita (2 * t)
dobraDuracao Infinita = Infinita

{-|
Responsável por ativar Inimigos de um Portal, transferindo Inimigos de uma onda do Portal para a lista de Inimigos ativos
-}
ativaInimigo :: Tempo -> Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo _ portal@(Portal {ondasPortal = []}) inimigos = (portal, inimigos)
ativaInimigo tempo portal@(Portal {posicaoPortal = pos, ondasPortal = onda@Onda {entradaOnda = entry, tempoOnda = time, cicloOnda = ciclo, inimigosOnda = inimigosOndaAtual} : resto}) inimigos =
  if null inimigosOndaAtual && entry <= 0 then
    -- A onda atual terminou, ativar a próxima onda, se existir
    let novoResto = case resto of
          (proxOnda : novasOndas) -> proxOnda {entradaOnda = max 0 (entradaOnda proxOnda - tempo)} : novasOndas
          [] -> []
    in (portal {ondasPortal = novoResto}, inimigos)
  else
    let
      -- Atualiza o tempo de entrada da onda atual
      novaEntradaOnda = max 0 (entradaOnda onda - tempo)
      ondaAtualizada = onda {entradaOnda = novaEntradaOnda}

      -- Lança um inimigo se o tempo da onda expira e a lista de inimigos não está vazia
      (portalAtualizado, novosInimigos) =
        if novaEntradaOnda == 0 && not (null inimigosOndaAtual) then
          let
            inimigoSemProjeteis = (head inimigosOndaAtual) {projeteisInimigo = []}

            ondaRestante = onda {inimigosOnda = tail inimigosOndaAtual, entradaOnda = ciclo}
          in (portal {ondasPortal = ondaRestante : resto}, inimigoSemProjeteis : inimigos)
        else
          (portal {ondasPortal = ondaAtualizada : resto}, inimigos)
    in
      (portalAtualizado, novosInimigos)

{-|
Decide se o jogo terminou
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

{-|
Verifica se o jogador ganhou o jogo
-}
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && vidaBase (baseJogo jogo) > 0 && all (all (null . inimigosOnda) . ondasPortal) (portaisJogo jogo)

{-|
Verifica se o jogador perdeu o jogo
-}
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0
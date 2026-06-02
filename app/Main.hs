module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tarefa1
import Tempo

{-|
Carrega todas as imagens que utilizamos no jogo
-}
carregarImagens :: IO Imagens
carregarImagens = do
               relva <- loadBMP "../2024LI1G017/app/imgs/relva.bmp"
               terra <- loadBMP "../2024LI1G017/app/imgs/terra.bmp"
               agua <- loadBMP "../2024LI1G017/app/imgs/agua.bmp"
               portal <- loadBMP "../2024LI1G017/app/imgs/portal.bmp"
               torregelo <- loadBMP "../2024LI1G017/app/imgs/torregelo.bmp"
               torrefogo <- loadBMP "../2024LI1G017/app/imgs/torrefogo.bmp"
               torreresina <- loadBMP "../2024LI1G017/app/imgs/torreresina.bmp"
               base <- loadBMP "../2024LI1G017/app/imgs/base.bmp"
               enemy_n <- loadBMP "../2024LI1G017/app/imgs/enemy_n.bmp"
               enemy_s <- loadBMP "../2024LI1G017/app/imgs/enemy_s.bmp"
               enemy_e <- loadBMP "../2024LI1G017/app/imgs/enemy_e.bmp"
               enemy_o <- loadBMP "../2024LI1G017/app/imgs/enemy_o.bmp"
               fogo <- loadBMP "../2024LI1G017/app/imgs/fogo.bmp"
               efogo <- loadBMP "../2024LI1G017/app/imgs/efogo.bmp"
               egelo <- loadBMP "../2024LI1G017/app/imgs/egelo.bmp"
               fundo_menu <- loadBMP "../2024LI1G017/app/imgs/fundo_menu.bmp"
               eresina <- loadBMP "../2024LI1G017/app/imgs/eresina.bmp"
               resina <- loadBMP "../2024LI1G017/app/imgs/resina.bmp"
               gelo <- loadBMP "../2024LI1G017/app/imgs/gelo.bmp"
               c50 <- loadBMP "../2024LI1G017/app/imgs/50.bmp"
               c100 <- loadBMP "../2024LI1G017/app/imgs/100.bmp"
               c150 <- loadBMP "../2024LI1G017/app/imgs/150.bmp"
               tfogo <- loadBMP "../2024LI1G017/app/imgs/Tfogotext.bmp"
               tgelo <- loadBMP "../2024LI1G017/app/imgs/Tgelotext.bmp"
               tresina <- loadBMP "../2024LI1G017/app/imgs/Tresinatext.bmp"
               loja <- loadBMP "../2024LI1G017/app/imgs/LOJA.bmp"
               jogar <- loadBMP "../2024LI1G017/app/imgs/jogar.bmp"
               sair <- loadBMP "../2024LI1G017/app/imgs/sair.bmp"
               loss <- loadBMP "../2024LI1G017/app/imgs/loss.bmp"
               win <- loadBMP "../2024LI1G017/app/imgs/win.bmp"
               jogardenovo <- loadBMP "../2024LI1G017/app/imgs/jogardenovo.bmp"
               sair2 <- loadBMP "../2024LI1G017/app/imgs/sair2.bmp"

               let imagens = [("relva", relva),
                              ("terra", terra),
                              ("agua", agua),
                              ("portal", portal),
                              ("torregelo", torregelo),
                              ("torrefogo", torrefogo),
                              ("torreresina", torreresina),
                              ("base", base),
                              ("enemy_n", enemy_n),
                              ("enemy_s", enemy_s),
                              ("enemy_e", enemy_e),
                              ("enemy_o", enemy_o),
                              ("fogo", fogo),
                              ("efogo", efogo),
                              ("egelo", egelo),
                              ("fundo_menu", fundo_menu),
                              ("eresina", eresina),
                              ("resina", resina),
                              ("gelo", gelo),
                              ("50", c50),
                              ("100", c100),
                              ("150", c150),
                              ("Tfogotext", tfogo),
                              ("Tgelotext", tgelo),
                              ("Tresinatext", tresina),
                              ("LOJA", loja),
                              ("jogar", jogar),
                              ("sair", sair),
                              ("loss", loss),
                              ("win", win),
                              ("jogardenovo", jogardenovo),
                              ("sair2", sair2)
                              ]
               return imagens

{-|
Função principal para executar o jogo
-}
main :: IO ()
main = do
  imagens <-  carregarImagens
  let
     it = ImmutableTowers {
      gameState = MenuState,
      menu = Menu {
        backgroundImage = undefined,
        buttonPlay = (-25, -110, 200, 50),
        buttonExit = (-25, -210, 200, 50)
      },
      initialState = if validaJogo estadoInicial then estadoInicial else undefined,
      images = imagens,
      mapaFormatado = transformarMapa mapa01 0,
      loja = lojaTorre,
      torreSelecionada = NenhumaTorre
    }
    in play
      FullScreen
      black
      60
      it
      desenha
      reageEventos
      reageTempo 

   
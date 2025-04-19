module Main (main) where

import Testes
import Lib
import Funcoes
import Tipos

main = do
  putStrLn "== Teste adicionar =="
  testaAdicionar

  putStrLn "\n== Teste remover =="
  testaRemover

  putStrLn "== Teste marcar como concluída =="
  testaMarcadaConcluida

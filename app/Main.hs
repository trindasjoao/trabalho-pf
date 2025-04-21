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

  putStrLn "\n== Teste marcar como concluída =="
  testaMarcadaConcluida
  
  putStrLn "\n== Teste: listar por categoria (Estudos) =="
  testaListarPorCategoria
  
  putStrLn "\n== Teste: listar tarefas por prioridade =="
  testaListarPorPrioridade

  putStrLn "\n== Teste: filtrar tarefas por status (Concluida) =="
  testaFiltrarPorStatusConcluidas

  putStrLn "\n== Teste: verificar tarefas atrasadas =="
  testaVerificarAtrasos

  putStrLn "== Teste: calcular dias restantes para a tarefa =="
  testaCalcularDiasRestantes

  putStrLn "== Teste: filtrar tarefas por tag =="
  testaFiltrarPorTag
  
  putStrLn "== Teste: gerar nuvem de tags =="
  testaNuvemDeTags

  putStrLn "== Teste: gerar relatório =="
  testaGerarRelatorio
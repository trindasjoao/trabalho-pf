module Main (main) where

import Testes
import Funcoes
import Tipos
import Persistencia

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

  putStrLn "\n== Teste: calcular dias restantes para a tarefa =="
  testaCalcularDiasRestantes

  putStrLn "\n== Teste: filtrar tarefas por tag =="
  testaFiltrarPorTag
  
  putStrLn "\n== Teste: gerar nuvem de tags =="
  testaNuvemDeTags

  putStrLn "\n== Teste: gerar relatório =="
  testaGerarRelatorio

  putStrLn "\n== Teste: salvar tarefas em arquivo =="
  testaSalvarEmArquivo

  putStrLn "\n== Teste: carregar tarefas do arquivo =="
  testaCarregarDeArquivo

  putStrLn "\n== Rodando QuickCheck em adicionarTarefa =="
  rodarQuickCheck
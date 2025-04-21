module Testes where 

import Tipos 
import Funcoes 
import Data.Time.Calendar (fromGregorian)
import Test.QuickCheck 

t1 = Tarefa 1 "Estudar Haskell" Pendente Alta Estudo (Just (fromGregorian 2025 04 20)) ["ufu", "haskell"]
t2 = Tarefa 2 "Fazer compras" Concluída Media Pessoal (Just (fromGregorian 2025 04 21)) ["casa"]
t3 = Tarefa 3 "Finalizar projeto" Pendente Alta Trabalho Nothing ["dev", "haskell"]

listaTarefas :: [Tarefa]
listaTarefas = [t1,t2,t3]

testaAdicionar :: IO ()
testaAdicionar = do
    let nova = Tarefa 4 "Nova tarefa" Pendente Baixa Outro Nothing ["nova"]
    case adicionaTarefa nova listaTarefas of 
        Left erro -> putStrLn $ "Erro: " ++ erro 
        Right lista -> do
            putStrLn "Tarefa adicionada!"
            print lista

testaRemover :: IO ()
testaRemover = do
  case removerTarefa t2 listaTarefas of
    Left erro -> putStrLn $ "Erro: " ++ erro
    Right lista -> do
      putStrLn "Tarefa removida com sucesso!"
      print lista

testaMarcadaConcluida :: IO ()
testaMarcadaConcluida = do
  let resultado = marcarConcluída 1 listaTarefas
  case resultado of
    Left erro -> putStrLn $ "Erro: " ++ erro
    Right novaLista -> do
      putStrLn "Tarefa marcada como concluída com sucesso!"
      print novaLista

testaListarPorCategoria :: IO ()
testaListarPorCategoria = do
  let resultado = listarPorCategoria Estudo listaTarefas
  putStrLn "Tarefas da categoria 'Estudos':"
  mapM_ print resultado


testaListarPorPrioridade :: IO ()
testaListarPorPrioridade = do
  let prioridadeDesejada = Alta
      resultado = listarPorPrioridade prioridadeDesejada listaTarefas
  putStrLn $ "Tarefas com prioridade " ++ show prioridadeDesejada ++ ":"
  print resultado

testaOrdenarPorPrioridade :: IO ()
testaOrdenarPorPrioridade = do
  let resultado = ordenarPorPrioridade listaTarefas
  putStrLn "Tarefas ordenadas por prioridade (Alta -> Média -> Baixa):"
  print resultado

testaFiltrarPorStatusConcluidas :: IO () --já fiz com concluída para não dar trabalho de colocar qual quer. 
testaFiltrarPorStatusConcluidas = do
  let resultado = filtrarPorStatus Concluída listaTarefas
  putStrLn "Tarefas concluídas:"
  print resultado

testaBuscarPorPalavraChave :: IO ()
testaBuscarPorPalavraChave = do
  let palavra = "haskell"
      resultado = buscarPorPalavraChave palavra listaTarefas
  putStrLn $ "Tarefas que contêm a palavra-chave '" ++ palavra ++ "' na descrição:"
  mapM_ print resultado

testaVerificarAtrasos :: IO ()
testaVerificarAtrasos = do
  let dataAtual = fromGregorian 2025 04 22  -- data simulada (hoje)
      resultado = verificarAtrasos listaTarefas dataAtual
  putStrLn $ "Tarefas atrasadas em relação a " ++ show dataAtual ++ ":"
  mapM_ print resultado

testaCalcularDiasRestantes :: IO ()
testaCalcularDiasRestantes = do
  let hoje = fromGregorian 2025 04 22
      tarefaExemplo = Tarefa 10 "Entregar projeto" Pendente Alta Estudo (Just (fromGregorian 2025 04 25)) ["projeto", "ufu"]
      resultado = calcularDiasRestantes tarefaExemplo hoje
  putStrLn $ "Dias restantes para a tarefa '" ++ descricao tarefaExemplo ++ "':"
  case resultado of
    Just dias -> putStrLn $ show dias ++ " dia(s)"
    Nothing   -> putStrLn "A tarefa não tem prazo definido."

testaFiltrarPorTag :: IO ()
testaFiltrarPorTag = do
  let tagBuscada = "haskell"
      resultado = filtrarPorTag tagBuscada listaTarefas
  putStrLn $ "Tarefas com a tag '" ++ tagBuscada ++ "':"
  mapM_ print resultado

testaNuvemDeTags :: IO ()
testaNuvemDeTags = do
  let resultado = nuvemDeTags listaTarefas
  putStrLn "Nuvem de tags (tag, quantidade de ocorrências):"
  mapM_ print resultado

testaGerarRelatorio :: IO ()
testaGerarRelatorio = do
  putStrLn (gerarRelatorio listaTarefas)
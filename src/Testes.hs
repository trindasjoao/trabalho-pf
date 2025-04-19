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
  let resultado = marcadaConcluida 1 listaTarefas
  case resultado of
    Left erro -> putStrLn $ "Erro: " ++ erro
    Right novaLista -> do
      putStrLn "Tarefa marcada como concluída com sucesso!"
      print novaLista

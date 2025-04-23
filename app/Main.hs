module Main (main) where

import System.IO ( hFlush, stdout )
import Data.Time.Calendar ( fromGregorian ) 
import Data.Time.Clock ( UTCTime(utctDay), getCurrentTime )
import Tipos
import Funcoes
import Persistencia
import Testes
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "\n\t\t\tGERENCIADOR DE TAREFAS PF\n"
  menuInicial []

menuInicial :: [Tarefa] -> IO ()
menuInicial tarefas = do
  putStrLn "\t\t\t\tMENU\n"
  putStr "1. Adicionar tarefa"
  putStrLn "\t\t\t2. Remover tarefa"
  putStr "3. Marcar tarefa como concluída"
  putStrLn "\t\t4. Listar tarefas por categoria"
  putStr "5. Listar tarefas por prioridade"
  putStrLn "\t6. Ordenar tarefas por prioridade"
  putStr "7. Filtrar tarefas por status"
  putStrLn "\t\t8. Buscar por palavra-chave"
  putStr "9. Verificar tarefas atrasadas"
  putStrLn "\t\t10. Filtrar por tag"
  putStr "11. Gerar nuvem de tags"
  putStrLn "\t\t\t12. Calcular quantos dias faltam para uma tarefa"
  putStr "13. Gerar relatório"
  putStrLn "\t\t\t14. Salvar em arquivo"
  putStr "15. Carregar de arquivo"
  putStrLn "\t\t\t16. QuickCheck"
  putStrLn "0. Sair"
  putStr "Escolha uma opção: "
  hFlush stdout
  opcao <- getLine
  executarOpcao opcao tarefas

executarOpcao :: String -> [Tarefa] -> IO ()
executarOpcao opcao tarefas = case opcao of
  "1" -> do
    nova <- criarTarefa
    menuInicial (nova : tarefas)
  "2" -> do
    putStrLn "Digite o ID da tarefa a remover:"
    hFlush stdout
    idStr <- getLine
    case readMaybe idStr of
      Just idt ->
        case removerTarefa idt tarefas of
          Left err      -> putStrLn err >> menuInicial tarefas
          Right novaLista -> menuInicial novaLista
      Nothing -> putStrLn "ID inválido!" >> menuInicial tarefas
  "3" -> do
    putStrLn "Digite o ID da tarefa a marcar como concluída:"
    hFlush stdout
    idStr <- getLine
    let idt = read idStr
    case marcarConcluída idt tarefas of
      Left err -> putStrLn err >> menuInicial tarefas
      Right novaLista -> menuInicial novaLista
  "4" -> do
    putStrLn "Digite a categoria (Estudo, Trabalho, Pessoal, Outro):"
    hFlush stdout
    cat <- getLine
    let categoriaSelecionada = read cat :: Categoria
    mapM_ print (listarPorCategoria categoriaSelecionada tarefas)
    menuInicial tarefas
  "5" -> do
    putStrLn "Digite a prioridade (Baixa, Media, Alta):"
    hFlush stdout
    p <- getLine
    let prio = read p :: Prioridade
    mapM_ print (listarPorPrioridade prio tarefas)
    menuInicial tarefas
  "6" -> do
    mapM_ print (ordenarPorPrioridade tarefas)
    menuInicial tarefas
  "7" -> do
    putStrLn "Digite o status (Pendente ou Concluída):"
    hFlush stdout
    s <- getLine
    let stat = read s :: Status
    mapM_ print (filtrarPorStatus stat tarefas)
    menuInicial tarefas
  "8" -> do
    putStrLn "Digite a palavra-chave:"
    hFlush stdout
    palavra <- getLine
    mapM_ print (buscarPorPalavraChave palavra tarefas)
    menuInicial tarefas
  "9" -> do
    hoje <- fmap utctDay getCurrentTime
    mapM_ print (verificarAtrasos tarefas hoje)
    menuInicial tarefas
  "10" -> do
    putStrLn "Digite a tag:"
    hFlush stdout
    tag <- getLine
    mapM_ print (filtrarPorTag tag tarefas)
    menuInicial tarefas
  "11" -> do
    let tags = nuvemDeTags tarefas
    mapM_ print tags
    menuInicial tarefas
  "12" -> do
    putStrLn "Digite o ID da tarefa:"
    hFlush stdout
    idStr <- getLine
    let idt = read idStr
    hoje <- fmap utctDay getCurrentTime
    case calcularDiasRestantes <$> (buscarPorId idt tarefas) <*> pure hoje of
      Just (Just dias) -> putStrLn ("Dias restantes: " ++ show dias)
      Just Nothing -> putStrLn "Tarefa sem prazo definido!!!"
      Nothing -> putStrLn "Tarefa não encontrada!!!"
    menuInicial tarefas
  "13" -> do
    putStrLn (gerarRelatorio tarefas)
    menuInicial tarefas
  "14" -> do
    putStrLn "Digite o nome do arquivo para salvar:"
    hFlush stdout
    arq <- getLine
    salvarEmArquivo arq tarefas
    putStrLn "Tarefas salvas!!!"
    menuInicial tarefas
  "15" -> do
    putStrLn "Digite o nome do arquivo:"
    hFlush stdout
    arq <- getLine
    tarefasNovas <- carregarDeArquivo arq
    putStrLn "Tarefas carregadas!!!"
    menuInicial tarefasNovas
  "16" -> do
    putStrLn "Rodando testes com QuickCheck..."
    rodarQuickCheck
    menuInicial tarefas  
  "0" -> putStrLn "Saindo..."
  _ -> putStrLn "Opção inválida!!!" >> menuInicial tarefas


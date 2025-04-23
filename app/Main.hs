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
  putStrLn "Bem-vindo ao Tarefas (Programação Funcional)"
  menuInicial []

menuInicial :: [Tarefa] -> IO ()
menuInicial tarefas = do
  putStrLn "\n========== MENU =========="
  putStrLn "1. Adicionar tarefa"
  putStrLn "2. Remover tarefa"
  putStrLn "3. Marcar tarefa como concluída"
  putStrLn "4. Listar tarefas por categoria"
  putStrLn "5. Listar tarefas por prioridade"
  putStrLn "6. Ordenar tarefas por prioridade"
  putStrLn "7. Filtrar tarefas por status"
  putStrLn "8. Buscar por palavra-chave"
  putStrLn "9. Verificar tarefas atrasadas"
  putStrLn "10. Filtrar por tag"
  putStrLn "11. Gerar nuvem de tags"
  putStrLn "12. Calcular dias restantes de uma tarefa"
  putStrLn "13. Gerar relatório"
  putStrLn "14. Salvar em arquivo"
  putStrLn "15. Carregar de arquivo"
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
    putStrLn "Digite o status (Pendente ou Concluida):"
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
      Just Nothing -> putStrLn "Tarefa sem prazo definido."
      Nothing -> putStrLn "Tarefa não encontrada."
    menuInicial tarefas
  "13" -> do
    putStrLn (gerarRelatorio tarefas)
    menuInicial tarefas
  "14" -> do
    putStrLn "Digite o nome do arquivo para salvar:"
    hFlush stdout
    arq <- getLine
    salvarEmArquivo arq tarefas
    putStrLn "Tarefas salvas!"
    menuInicial tarefas
  "15" -> do
    putStrLn "Digite o nome do arquivo para carregar:"
    hFlush stdout
    arq <- getLine
    tarefasNovas <- carregarDeArquivo arq
    putStrLn "Tarefas carregadas!"
    menuInicial tarefasNovas
  "0" -> putStrLn "Saindo..."
  _ -> putStrLn "Opção inválida!" >> menuInicial tarefas

buscarPorId :: Int -> [Tarefa] -> Maybe Tarefa
buscarPorId ident = foldr (\t acc -> if idTarefa t == ident then Just t else acc) Nothing

criarTarefa :: IO Tarefa
criarTarefa = do
  putStrLn "ID:"
  hFlush stdout
  idStr <- getLine
  putStrLn "Descrição:"
  hFlush stdout
  desc <- getLine
  putStrLn "Status (Pendente ou Concluida):"
  hFlush stdout
  stat <- getLine
  putStrLn "Prioridade (Baixa, Media, Alta):"
  hFlush stdout
  prio <- getLine
  hFlush stdout
  putStrLn "Categoria (Estudos, Trabalho, Pessoal, Outro):"
  hFlush stdout
  cat <- getLine
  putStrLn "Deseja adicionar prazo? (s/n)"
  hFlush stdout
  r <- getLine
  prazo <- if r == "s"
    then do
      putStrLn "Digite o prazo (DD MM YYYY):"
      hFlush stdout
      d <- getLine
      let [dia, m, y] = map read (words d)
      return (Just (fromGregorian (fromIntegral y) m dia))
    else return Nothing
  putStrLn "Digite as tags separadas por espaço:"
  hFlush stdout
  tagStr <- getLine
  let tagsLista = words tagStr
  return (Tarefa (read idStr) desc (read stat) (read prio) (read cat) prazo tagsLista)
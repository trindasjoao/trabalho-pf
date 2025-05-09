module Testes where 

import Tipos 
import Funcoes 
import Persistencia
import Data.Time.Calendar (Day, fromGregorian)
import Test.QuickCheck 
import qualified Data.Set as Set

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
  case removerTarefa (idTarefa t2) listaTarefas of
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

arquivoTeste :: FilePath
arquivoTeste = "tarefas_teste.txt"

testaSalvarEmArquivo :: IO ()
testaSalvarEmArquivo = do
  putStrLn "Salvando tarefas em arquivo..."
  salvarEmArquivo "tarefas_teste.txt" listaTarefas
  putStrLn "Arquivo salvo com sucesso!"

testaCarregarDeArquivo :: IO ()
testaCarregarDeArquivo = do
  putStrLn "Carregando tarefas do arquivo..."
  tarefas <- carregarDeArquivo "tarefas_teste.txt"
  putStrLn "Tarefas carregadas:"
  mapM_ print tarefas

instance Arbitrary Status where
  arbitrary = elements [Pendente, Concluída]

instance Arbitrary Prioridade where
  arbitrary = elements [Baixa, Media, Alta]

instance Arbitrary Categoria where
  arbitrary = elements [Estudo, Trabalho, Pessoal, Outro]

instance Arbitrary Tarefa where
  arbitrary = do
    idTarefa <- arbitrary
    descricao <- arbitrary
    status <- arbitrary
    prioridade <- arbitrary
    categoria <- arbitrary
    prazo <- frequency [(1, return Nothing), (3, Just <$> arbitrary)]
    tags <- listOf arbitrary
    return (Tarefa idTarefa descricao status prioridade categoria prazo tags)

prop_adicionaTarefaAumentaLista :: Tarefa -> [Tarefa] -> Property
prop_adicionaTarefaAumentaLista t ts =
  Set.notMember (idTarefa t) (Set.fromList (map idTarefa ts)) ==>
    case adicionaTarefa t ts of
      Right novaLista -> length novaLista == length ts + 1
      Left _          -> False

rodarQuickCheck :: IO ()
rodarQuickCheck = do
  putStrLn "== Rodando QuickCheck em adicionarTarefa =="
  quickCheck prop_adicionaTarefaAumentaLista

instance Arbitrary Day where
  arbitrary = fromGregorian
    <$> choose (2020, 2030)  -- ano
    <*> choose (1, 12)       -- mês
    <*> choose (1, 28)       -- dia (mais seguro que 30 ou 31)


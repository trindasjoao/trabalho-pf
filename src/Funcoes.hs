module Funcoes where 

import Tipos
import Data.Time.Calendar (diffDays, Day, fromGregorian)
import Data.List
import Data.Char
import Control.Arrow (ArrowChoice(right))

existeId :: Int -> [Tarefa] -> Bool
existeId ident xs = any (\x -> idTarefa x == ident) xs

adicionaTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionaTarefa ident tarefas
  |existeId (idTarefa ident) tarefas = Left "Erro -> tarefa já existe!!"
  |otherwise = Right (ident : tarefas)  

removerTarefa :: Int -> [Tarefa] -> Either String [Tarefa]
removerTarefa ident tarefas
  | existeId ident tarefas = Right (filter (\t -> idTarefa t /= ident) tarefas)
  | otherwise = Left "Erro -> Tarefa não existe!!!"


marcarConcluída :: Int -> [Tarefa] -> Either String [Tarefa]
marcarConcluída ident tarefas 
    |not (existeId ident tarefas) = Left "Erro -> Tarefa não existe!!"
    |otherwise = Right(map atualizar tarefas)
    where
      atualizar t 
        |idTarefa t == ident = t {status = Concluída}
        |otherwise = t

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cate tarefas = [t | t <- tarefas, categoria t == cate] --t é a tarefa. Percorre cada t em um conjunto de tarefas, e só inclui na nova lista os t que satisfazem a condição, ou seja, cuja categoria seja igual a cat

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prio tarefas = [t | t <- tarefas, prioridade t == prio] --mesma lógica da função que lista por categoria.

ordenarPorPrioridade :: [Tarefa] -> [Tarefa] 
ordenarPorPrioridade tarefas = 
    let altas  = [t | t <- tarefas, prioridade t == Alta] --separa as tarefas em 3 lista, e depois concatena elas
        medias = [t | t <- tarefas, prioridade t == Media]
        baixas = [t | t <- tarefas, prioridade t == Baixa]
    in altas ++ medias ++ baixas

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa] 
filtrarPorStatus stat tarefas = [t | t <- tarefas, status t == stat] --mesma lógica da função que lista por categoria 

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave palavra tarefas = [t | t <- tarefas, map toLower palavra `isInfixOf` map toLower (descricao t)] -- seguindo a lógica de compreensão de listas, percorre cada tarefa em um conjunto de tarefas, e entra na condição. toLower faz com que cada caractere da palavra passe a ser minúsculo para não ter conflito na hora de procurar pela palavra-chave. 
-- isInfixOf verifica se a palavra está contida dentro da descrição, que também já foi passada para minúscula para não haver conflito

verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos tarefas hoje = filter atrasada tarefas
  where 
    atrasada t = case prazo t of 
      Just d -> d < hoje && status t /= Concluída
      Nothing -> False 

calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes t hoje = case prazo t of
    Just d -> -- Se a tarefa tem um prazo (Just d)
        Just (fromInteger(diffDays d hoje)) -- Calcula a diferença de dias entre a data atual (hoje) e o prazo da tarefa (d), a função diffDays retorna a quantidade de dias entre essas duas datas, o resultado será do tipo Maybe Int, que contém o número de dias restantes
    Nothing -> Nothing -- Se a tarefa não tem prazo (Nothing), retorna Nothing, indicando que não é possível calcular os dias restantes

-- Retorna todas as tarefas que contêm uma tag específica
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
filtrarPorTag tag tarefas =
  filter (\t -> tag `elem` tags t) tarefas -- Verifica se a tag está presente na lista de tags da tarefa


-- Gera uma lista de tags com suas frequências de uso
nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags tarefas =
  let todasAsTags = concatMap tags tarefas -- Junta todas as listas de tags em uma só
  in map (\tg -> (head tg, length tg)) . group . sort $ todasAsTags -- Agrupa e conta as repetições de cada tag

gerarRelatorio :: [Tarefa] -> String -- Função para gerar o relatório resumido
gerarRelatorio tarefas = unlines [
    "Relatório Resumido:",
    "-------------------",
    "Total de tarefas: " ++ show totalTarefas,
    "Pendentes: " ++ show pendentes ++ " | Concluídas: " ++ show concluidas,
    "Distribuição por categoria:",
    "  Estudo: " ++ show estudo ++ " tarefa(s) (" ++ show (percentual estudo) ++ "%)",
    "  Trabalho: " ++ show trabalho ++ " tarefa(s) (" ++ show (percentual trabalho) ++ "%)",
    "  Pessoal: " ++ show pessoal ++ " tarefa(s) (" ++ show (percentual pessoal) ++ "%)",
    "  Outro: " ++ show outro ++ " tarefa(s) (" ++ show (percentual outro) ++ "%)"
  ]
  where
    totalTarefas = length tarefas  -- Contagem total de tarefas

    pendentes = length [t | t <- tarefas, status t == Pendente]  -- Contagem de tarefas pendentes e concluídas
    concluidas = length [t | t <- tarefas, status t == Concluída]
    estudo = length [t | t <- tarefas, categoria t == Estudo]   -- Contagem de tarefas por categoria
    trabalho = length [t | t <- tarefas, categoria t == Trabalho]
    pessoal = length [t | t <- tarefas, categoria t == Pessoal]
    outro = length [t | t <- tarefas, categoria t == Outro]

    percentual :: Int -> Double  -- Função para calcular o percentual de tarefas por categoria
    percentual count = (fromIntegral count / fromIntegral totalTarefas) * 100

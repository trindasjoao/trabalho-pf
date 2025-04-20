module Funcoes where 

import Tipos
import Data.Time.Calendar (fromGregorian)
import Control.Arrow (ArrowChoice(right))

existeId :: Int -> [Tarefa] -> Bool
existeId ident xs = any (\x -> idTarefa x == ident) xs

adicionaTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionaTarefa ident tarefas
  |existeId (idTarefa ident) tarefas = Left "Erro -> tarefa já existe!!"
  |otherwise = Right (ident : tarefas)  

removerTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
removerTarefa ident tarefas
  |existeId (idTarefa ident) tarefas = Right (filter (\t -> idTarefa t /= idTarefa ident) tarefas)
  |otherwise = Left "Erro -> Tarefa não existe!!!"

marcarConcluída :: Int -> [Tarefa] -> Either String [Tarefa]
marcarConcluída ident tarefas 
    |not (existeId ident tarefas) = Left "Erro -> Tarefa não existe!!"
    |otherwise = (map atualizar tarefas)
    where
      atualizar t 
        |idTarefa t == ident = t {status = Concluída}
        |otherwise = t

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cate tarefas = [t | t <- tarefas, categoria t == cate] //t é a tarefa. Percorre cada t em um conjunto de tarefas, e só inclui na nova lista os t que satisfazem a condição, ou seja, cuja categoria seja igual a cat

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prio tarefas = [t | t <- tarefas, prioridade t == prio] //mesma lógica da função que lista por categoria.

ordenarPorPrioridade :: [Tarefa] -> [Tarefa] 
ordenarPorPrioridade tarefas = 
    let altas  = [t | t <- tarefas, prioridade == Alta] //separa as tarefas em 3 lista, e depois concatena elas
        medias = [t | t <- tarefas, prioridade == Media]
        baixas = [t | t <- tarefas, prioridade == Baixa]
    in altas ++ medias ++ baixas

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa] 
filtrarPorStatus stat tarefas = [t | t <- tarefas, status t = stat] //mesma lógica da função que lista por categoria 

-- A função recebe uma lista de tarefas, a data atual e retorna uma lista de tarefas que estão atrasadas (com o prazo expirado).
verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos tarefas hoje = filter (\t -> case prazo t of
    -- Verifica se a tarefa tem prazo (Just d)
    Just d -> 
        -- Se o prazo da tarefa for antes da data atual e o status não for Concluída, mantém a tarefa na lista
        d < hoje && status t /= Concluída
    -- Se a tarefa não tem prazo (Nothing), não considera a tarefa atrasada
    Nothing -> False) tarefas

calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes t hoje = case prazo t of
    -- Se a tarefa tem um prazo (Just d)
    Just d -> 
        -- Calcula a diferença de dias entre a data atual (hoje) e o prazo da tarefa (d)
        -- A função diffDays retorna a quantidade de dias entre essas duas datas
        -- O resultado será do tipo Maybe Int, que contém o número de dias restantes
        Just (diffDays d hoje)
    -- Se a tarefa não tem prazo (Nothing), retorna Nothing, indicando que não é possível calcular os dias restantes
    Nothing -> Nothing

-- Retorna todas as tarefas que contêm uma tag específica
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
filtrarPorTag tag tarefas =
  filter (\t -> tag `elem` tags t) tarefas -- Verifica se a tag está presente na lista de tags da tarefa


-- Gera uma lista de tags com suas frequências de uso
nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags tarefas =
  let todasAsTags = concatMap tags tarefas -- Junta todas as listas de tags em uma só
  in map (\tg -> (head tg, length tg)) . group . sort $ todasAsTags -- Agrupa e conta as repetições de cada tag


-- Salva a lista de tarefas em um arquivo no formato texto
salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo caminho tarefas = do
  let conteudo = unlines (map show tarefas) -- Converte a lista de tarefas para string com quebras de linha
  writeFile caminho conteudo -- Escreve o conteúdo no arquivo


-- Carrega a lista de tarefas de um arquivo texto
carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo caminho = do
  conteudo <- readFile caminho -- Lê o conteúdo do arquivo
  return (map read (lines conteudo)) -- Converte cada linha em uma Tarefa

-- Filtra tarefas que contenham todas as tags fornecidas
filtrarPorTags :: [String] -> [Tarefa] -> [Tarefa]
filtrarPorTags ts tarefas = filter (\t -> all (`elem` tags t) ts) tarefas
-- A função recebe uma lista de tags 'ts' e uma lista de tarefas
-- Para cada tarefa, verifica se todas as tags da lista 'ts' estão presentes nas tags da tarefa (usando 'all')
-- Apenas as tarefas que tiverem todas as tags são mantidas na lista final



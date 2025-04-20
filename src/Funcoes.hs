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

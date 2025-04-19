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
    |otherwise (map atualizar tarefas)
    where
      atualizar t 
        |idTarefa t == ident = t {status = Concluída}
        |otherwise = t

module Funcoes where 

import Tipos
import Data.Time.Calendar (fromGregorian)

existeId :: Int -> [Tarefa] -> Bool
existeId iden xs = any (\x -> idTarefa x == iden) xs

adicionaTarefa :: Tarefa -> [Tarefa] -> Either String [Tarefa]
adicionaTarefa nova tarefas
  |existeId (idTarefa nova) tarefas = Left "Erro -> tarefa já existe!!"
  |otherwise = Right (nova : tarefas)  

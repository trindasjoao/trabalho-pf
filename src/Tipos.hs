module Tipos where

import Data.Time.Calendar

data Status = Pendente | Concluída deriving (Show, Eq)

data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord)

data Categoria = Trabalho | Estudo | Pessoal | Outro deriving (Show, Eq)

data Tarefa = Tarefa {
    idTarefa :: Int,
    descricao :: String,
    status :: Status,
    prioridade :: Prioridade,
    categoria :: Categoria,
    prazo :: Maybe Day,
    tags :: [String]
} deriving (Show, Eq)
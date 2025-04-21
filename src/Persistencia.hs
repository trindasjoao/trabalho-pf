module Persistencia where 

import Tipos

salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo caminho tarefas = writeFile caminho (unlines (map show tarefas))


carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo caminho = do
  conteudo <- readFile caminho
  let linhas = lines conteudo
  let tarefas = map read linhas
  return tarefas


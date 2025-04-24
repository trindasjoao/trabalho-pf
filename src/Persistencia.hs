module Persistencia where 

import Tipos
import Text.Read(readMaybe)

salvarEmArquivo :: FilePath -> [Tarefa] -> IO ()
salvarEmArquivo caminho tarefas = writeFile caminho (unlines (map show tarefas))

carregarDeArquivo :: FilePath -> IO [Tarefa]
carregarDeArquivo caminho = do
  conteudo <- readFile caminho
  let linhas = lines conteudo
  let tarefas = map parseLinha linhas
  mapM_ (\(i, res) -> case res of
          Nothing -> putStrLn $ "Erro na linha " ++ show i ++ ": " ++ linhas !! i
          _ -> return ()) (zip [0..] tarefas)
  return [t | Just t <- tarefas]

parseLinha :: String -> Maybe Tarefa
parseLinha = readMaybe
# Trabalho de Programação Funcional!

Projeto realizado em grupo da matéria Programação Funcional, cujo objetivo era criar um gerenciador de tarefas. O projeto foi feito em Haskell em grupo. 

## Funcionalidades do projeto

-> Adicionar, remover e listar tarefas;
-> Marcar tarefas como concluídas;
-> Filtrar tarefas por status, categoria, prioridade e tags;
-> Buscar;
-> Verificar prazos e atrasos;
-> Salvar e carregar arquivos de texto;
-> Gerar um relatório geral de tarefas;
-> Menu interativo
-> Teste com QuickCheck

## Estrutura do Projeto 

-> app/Main.hs
    Menu interativo completo com todas as funções ditas acima.

-> src/Tipos.hs
    Função que contém os tipos de dados.

-> src/Funcoes.hs
    Função que contém as funções que manipulam as tarefas. 

-> src/Persistencia.hs
    Função que lê e grava em arquivos .txt.

-> src/Testes.hs
    Função utilizada durante criação de cada função, além do QuickCheck, as outras funções não são utilizadas no projeto final, mas foram usadas para validar todas as funções do código durante construção do projeto. 

-> tarefas_texte.txt
    Exemplo de arquivo de texto para leitura de Tarefas.

## Como rodar na sua máquina 

1. Você deve possuir instalado o [Haskell] (https://www.haskell.org/ghcup/);
2. Você deverá clonar o repositório no terminal com as respectivas funções em sequência...

```bash
git clone https://github.com/trindasjoao/trabalho-pf
cd trabalho-pf 
stack build 
stack run

```

## Autores 
    João Pedro Trindade Melo - 12411BCC084 
    Maria Clara -  
    Kaio Phelipe Silva Machado - 12421BCC058

## Licença 

Projeto feito para fins acadêmicos na disciplica de Progamação Funcional do curso de Ciência da Computação da Faculdade Federal de Uberlândia (UFU).






# Trabalho de Programação Funcional!

Projeto realizado em grupo da matéria Programação Funcional, cujo objetivo era criar um gerenciador de tarefas. O projeto foi feito em Haskell em grupo. 

## Funcionalidades do projeto

-> Adicionar, remover e listar tarefas; <br>
-> Marcar tarefas como concluídas;<br>
-> Filtrar tarefas por status, categoria, prioridade e tags;<br>
-> Buscar;<br>
-> Verificar prazos e atrasos;<br>
-> Salvar e carregar arquivos de texto;<br>
-> Gerar um relatório geral de tarefas;<br>
-> Menu interativo<br>
-> Teste com QuickCheck<br>

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

Após isso feito, inicie o programa e crie tarefas com a opção do menu 1, a maioria das funções necessitam que as tarefas sejam criadas. Caso o usuário prefira, neste arquivo tem um arquivo .txt já preparado. Você deverá iniciar o programa (como indicado acima) e usar a opção 15 para carregar o arquivo desejado só inserindo seu nome. Após carregado a maioria das funções carregarão normalmente. Aproveite :) 

## Autores 
    João Pedro Trindade Melo - 12411BCC084 
    Maria Clara -  12411BCC106
    Kaio Phelipe Silva Machado - 12421BCC058

## Licença 

Projeto feito para fins acadêmicos na disciplica de Progamação Funcional do curso de Ciência da Computação da Faculdade Federal de Uberlândia (UFU).






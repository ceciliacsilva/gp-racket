Genetic Programming
-----------------

Esse repositório é meu "baby step" em programação genética (GP), os programas foram escritos em Racket Language. Esse projeto é uma mistura dos conceito desse blog(http://aerique.blogspot.com.br/2011/01/baby-steps-into-genetic-programming.html) com algumas adaptações das minhas aulas sobre algorítmos genéticos. "Ceci n'est pas un tutorial" são alguns testes.

"In genetic programming we evolve a population of computer programs. That is, generation by generation, GP stochastically transforms populations of programs into new, hopefully better, populations of programs. GP, like nature, is a random process, and it can never guarantee results. GP's essential randomness, however, can lead it to escape traps which deterministic methods may be captured by. Like nature, GP has been very successful at evolving novel and unexpected ways of solving problems." - A Field Guide to Genetic Programming (ISBN 978-1-4092-0073-4)

Tipicamente em algoritmos evolutivos (AEs), entre eles GP, a população inicial é normalmente gerada de maneira aleatória. Existem diferentes caminho para isso, nesse caso foi usado Ramped half-and-half, combinação entre os métodos Grow e Full.

Objetivo é a partir do Input e do Target, Input = '(1 2 3 4) e Target = '(1 4 9 16) encontrar a melhor formula possível. Os individuos são classificados usando uma função de fitness. 

* O critério de parada do algoritmo é descrito na estrutura - gp-finalizar.

      (define-struct gp-finalizar
      		     (fitness-min repeticoes))
    ;;até atingir "fitness-min" ou repetir o melhor fitness por "repeticoes"

* Parametros da população estrutura - gp-parametros

      (define-struct gp-parametros
      		       (max-populacao taxa-mutacao chance-mutacao max-depth))
    ;;max-depth máxima profundidade de uma formula
    ;;taxa-mutacao e chance-mutacao usados para mutacionar a população
    ;;max-populacao tamanho da população

Casos de teste
-------------

Criando um gp, nome do diretório de trabalho = "teste1".

	> ;;(gp-run-funcao nome-dir min max step funcao [operadores *operadores*]
	                       #:gp-valor [gp-valores gp-padrao])
	> (define gp1 (gp-run-funcao "teste1" 1 4 1 (lambda(a)  (* a a)))) ;;cria o arquivo 'log'

	Input  = lista min até max de step, ex: min=1 max=4 step=1 '(1 2 3 4)
	Target = lista funcao(x), para x de min até max de step, ex: min=1 max=4 step=1 '(1 4 9 16)

	;;Gerando relatório

	> (gp1)  ;;cria o arquivo 'gp.xml'

	;;Testando um valor, na formula encontrada

	> (gp1 #:point 3.5)
	12.25

	;;Plotando o gráfico da formula encontrada

	> (gp1 #:min-plot 0 #:max-plot 10)

Emacs Interface
----------

Simples interface. Seguir instruções "emacs-interface.org".

No final do bloco 'gp-interface'

	 ;;(gp-main <nome-projeto> <tabela-dados> <campos-dados> <coluna-input> <coluna-output>)
	 ;;ex:
	 ;;(gp-main "teste5" gp-dados gp-campos "input" "target")

	 ;;C-c C-c
	 ;;Evaluate this emacs-lisp code block (gp-interface) on your system? (yes or no) yes

Usar os dados no programa:

	 ;;> (define gp2 (gp-arq "teste5"))
	 ;;> (gp2)
	 ;;> (gp2 #:point 7)
	 ;;14
	 ;;> (gp2 #:min-plot 0 #:max-plot 10)
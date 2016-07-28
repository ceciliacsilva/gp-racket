#lang racket


;;Cecília Carneiro e Silva
;;Graduanda em Engenharia Elétrica - Universidade Federal de Uberlândia
;;--------------
;;Genetic Programming em Racket Language, primeira experiência

;;Usos:
;;> (define gp (gp-run-funcao "teste1" 1 4 1 (lambda(a)  (* a a))))
;;> (gp)
;;> (gp #:min-plot 0 #:max-plot 20)
;;> (define gp-outros-parametros (make-gp-parametros 130 20 60 4))
;;> (define gp1 (gp-run-funcao "teste6" 1 4 1 (lambda(a)  (* a a)) #:gp-valor gp-outros-parametros))
;;> (gp1)
;;> (gp1 #:point 20)

;;Alterar os criterios de finalizacao: mudar a estrutura 'gp-fim'
;;ex:
;;(set! gp-fim (make-gp-fim 0.98 200))

;;Relatorio gerado: gp.xml, dentro da pasta do projeto.

(require plot)
(plot-new-window? #t)

(define (funcall fun . args)
  (apply fun args))

(define-struct gp-parametros
  (max-populacao taxa-mutacao chance-mutacao max-depth))

(define-struct no-parametros
  (index no))

(define-struct individuo-parametros
  (fitness formula))

(define-struct populacao-parametros
  (porcentagem individuo))

(define-struct gp-finalizar
  (fitness-min repeticoes))

(define gp-fim (make-gp-finalizar 0.95 200))

(define gp-padrao (make-gp-parametros 100 25 50 4))

(define gp-max-manter (lambda(gp-valores)
                        (/ (gp-parametros-max-populacao gp-valores) 2)))

(define *operadores* '(+ - * / expt))

(define *dir-output* "output/")

(define (elemento-par numero)
  (cond
   ((zero? numero) 2)
   ((odd? numero) (+ numero 1))
   (else numero)))

(define (elemento-nao-zero numero)
  (cond ((zero? numero) 1)
        (else numero)))

(define (elemento-random lista #:posicao? [retorna-posicao #f])
  (let ((tamanho (length lista)))
    (cond ((> tamanho 0)
           (let* ((posicao (random tamanho))
                  (elemento (list-ref lista posicao)))
             (if retorna-posicao
                 (values posicao elemento)
                 elemento
                 )
             ))
          ))
  )

(define (formula-random-grow operadores #:max [gp-valores gp-padrao])
  (let ((max-depth (gp-parametros-max-depth gp-valores)))
    (let ((taxa1 50)
          (taxa2 75))
      (let loop ((profundidade max-depth))
        (let ((operador (elemento-random operadores)))
          (append (list operador)              
                  (for/list ((i (in-range 2)))
                    (let ((probabilidade (random 100)))
                      (if (> profundidade 0)
                          (cond ((< probabilidade taxa1)
                                 (loop (- profundidade 1)))
                                ((< probabilidade taxa2)
                                 (* 10.0 (random)))
                                (else '=input=))
                          (cond ((< probabilidade taxa1)
                                 (* 10.0 (random)))
                                (else '=input=))
                          )
                      ))
                  )
          )))
    )
  )

(define (formula-random-full operadores #:max [gp-valores gp-padrao])
  (let ((max-depth (gp-parametros-max-depth gp-valores)))
    (let ((taxa1 50))
      (let loop ((profundidade max-depth))
        (let ((operador (elemento-random operadores)))
          (append (list operador)
                  (for/list ((i (in-range 2)))
                    (if (> profundidade 0)
                        (loop (- profundidade 1))
                        (let ((probabilidade (random 100)))
                          (cond ((< probabilidade taxa1)
                                 (* 10.0 (random)))
                                (else '=input=))
                          )
                        )
                    ))
          )))
    )
  )

(define (formula-run formula input)
  (with-handlers ([number? (lambda(v) v)]
                  [exn:fail? (lambda(v) '())])
    (funcall (eval `(lambda(=input=) ,formula)) input))
  )

(define (populacao-criar-inicial operadores [gp-valores gp-padrao])
    (let* ((tamanho (gp-parametros-max-populacao gp-valores))
           (populacao-tamanho (elemento-par tamanho)))
      (append
       (for/list ((i (in-range (/ populacao-tamanho 2))))
        (formula-random-grow operadores #:max gp-valores))
       (for/list ((i (in-range (/ populacao-tamanho 2))))
        (formula-random-full operadores #:max gp-valores))
       ))
    )

(define (individuo-fitness formula input-lista output-lista)
  (with-handlers ([number? (lambda(v) v)]
                  [exn:fail? (lambda(v) '())])
    (apply *
           (for/list ((input  (in-list input-lista))
                      (target (in-list output-lista)))
             (let ((output (formula-run formula input)))
               (cond ((not (null? output))
                      (let* ((diferenca (abs (- target output)))
                             (fitness   (/ 1.0 (+ 1 diferenca))))
                        fitness))
                     ))
             ))
    )
  )

(define (nos-quantidade formula)
  (let ((nos 0))
    (let loop ((subformula formula))
      (for ((no (in-list subformula)))
        (cond ((pair? no)
               (loop no))
              (else (set! nos (+ nos 1)))
              )
        ))
    nos))

(define (nos-random formula)
  (let* ((index 1)
        (nos-qtdd (nos-quantidade formula))
        (nos-random-index (+ (random (- nos-qtdd 1)) 1))
        (no-escolhido '()))
    (let loop ((subformula formula))
      (for ((no (in-list subformula)))
        (cond ((= index nos-random-index)
               (set! no-escolhido (make-no-parametros index no)))
              ((pair? no) (loop no))
              (else
               (set! index (+ index 1)))
              ))
      )
    no-escolhido))

(define (lista-replace lista novo-index novo-lista)
  (let ((index 0))
    (let loop ((sublista lista))
      (for/list ((no (in-list sublista)))
        (set! index (+ index 1))
        (cond ((= index novo-index)
               novo-lista)
              ((not (pair? no))
               no)
              (else (loop no)))
        ))
    ))

(define (individuo-crossover formula1 formula2 #:debug? [debug #f])
  (let ((no-formula1 (nos-random formula1))
        (no-formula2 (nos-random formula2)))
    (when debug
      (displayln (~a "formula1: ~S~%formula2: ~S~%no-formula1: ~S~%no-formula2: ~S~%"
	      formula1 formula2 no-formula1 no-formula2)))
    (let ((formula1-index  (no-parametros-index no-formula1))
          (formula1-no-esc (no-parametros-no no-formula1))
          (formula2-index  (no-parametros-index no-formula2))
          (formula2-no-esc (no-parametros-no no-formula2)))
      (let ((filho1 (lista-replace formula1
                                   formula1-index formula2-no-esc))
            (filho2 (lista-replace formula2
                                   formula2-index formula1-no-esc)))
        (values filho1 filho2)))
    ))

(define (individuo-mutacao formula operadores #:debug? [debug #f] #:max [gp-valores gp-padrao])
  (let* ((no-mutacao   (nos-random formula))
         (no-novo      (formula-random-grow operadores #:max gp-valores))
         (no-mut-index (no-parametros-index no-mutacao)))
    (when debug
      (displayln (~a "antigo: ~S~%parte modificada: ~S~%parte nova: ~S~%" formula no-mutacao no-novo)))
    (lista-replace formula no-mut-index no-novo))
  )

(define (populacao-ordena populacao input-lista output-lista)
  (let loop ((individuo (car populacao))
             (pop-nova  (cdr populacao))
             (pop-final '()))
    (if (not (null? pop-nova))
        (let ((fitness (individuo-fitness individuo input-lista output-lista)))
          (cond ((number? fitness)       
                 (loop (car pop-nova) (cdr pop-nova)
                       (cons (make-individuo-parametros fitness individuo) pop-final)))
                (else (loop (car pop-nova) (cdr pop-nova) pop-final))
                ))
        (sort pop-final (lambda(a b)
                          (let ((a-fitness (individuo-parametros-fitness a))
                                (b-fitness (individuo-parametros-fitness b)))
                            (> a-fitness b-fitness))))
        ))
  )

(define (lista-head lista [amount 1])
  (if (<= amount 0)
      '()
      (if (< (length lista) amount)
          lista
          (take lista amount)
          ))
  )

(define (populacao-criar-roleta populacao)
  (let* ((lista-fitness (map (lambda(ind) (individuo-parametros-fitness ind)) populacao))
         (soma (apply + lista-fitness)))
    (let ((acc 0))
      (for/list ((individuo (in-list populacao)))
        (let ((fitness (individuo-parametros-fitness individuo))
              (formula (individuo-parametros-formula individuo)))
          (set! acc (+ (/ fitness soma) acc))
          (make-populacao-parametros (+ (/ fitness soma) acc) formula)
          )
        ))
    )
  )

(define (populacao-random-roleta roleta [roleta? #t])
  (let ((valor (random)))
    (if roleta?
        (let loop ((pop roleta))
          (cond ((not (null? pop))
                 (let* ((ind (car pop))
                        (porcentagem (populacao-parametros-porcentagem ind)))
                   (if (< valor porcentagem)
                       (populacao-parametros-individuo ind)
                       (loop (cdr pop)))
                   ))
                )
          )
        (populacao-parametros-individuo (elemento-random roleta))
        )
    )
  )

(define (populacao-proxima-geracao populacao input-lista output-lista operadores #:gp-valor [gp-valores gp-padrao])
  (let* ((max-populacao  (gp-parametros-max-populacao gp-valores))
         (taxa-mutacao   (gp-parametros-taxa-mutacao gp-valores))
         (chance-mutacao (gp-parametros-chance-mutacao gp-valores))
         (max-manter     (gp-max-manter gp-valores)))
    (let* ((populacao-roleta  (populacao-criar-roleta populacao))
           (elementos-manter  (elemento-par (random max-manter)))
           (individuos-manter (map (lambda(a) (individuo-parametros-formula a))
                                   (lista-head populacao elementos-manter)))
           (nro-cruzamentos (/ (- max-populacao elementos-manter) 2)))
      (let ((populacao-cruzamentos
             (let loop ((index nro-cruzamentos) (pop-final '()))
               (let ((individuo1 (populacao-random-roleta populacao-roleta))
                     (individuo2 (populacao-random-roleta populacao-roleta)))
                 (let-values (((filho1 filho2) (individuo-crossover individuo1 individuo2)))
                   (if (> index 0)
                       (let ((filho-individuo1 (if (number? (formula-run filho1 (car input-lista)))
                                                   filho1
                                                   (formula-random-grow operadores #:max gp-valores)))
                             (filho-individuo2 (if (number? (formula-run filho2 (car input-lista)))
                                                   filho2
                                                   (formula-random-grow operadores #:max gp-valores))))
                         (loop (- index 1) (cons
                                            filho-individuo1
                                            (cons filho-individuo2 pop-final)))
                         )
                       pop-final
                       )
                   )
                 ))
               ))
        (let loop ((vezes chance-mutacao) (pop-mutacao populacao-cruzamentos))
          (let ((valor (random 100)))
            (let-values (((posicao-mutacao elemento-mutacao) (elemento-random pop-mutacao #:posicao? #t)))
              (let ((individuo-novo (individuo-mutacao elemento-mutacao operadores)))
                (if (> vezes 0)
                    (loop (- vezes 1) 
                          (cond ((and (< valor taxa-mutacao) (formula-run individuo-novo (car input-lista)))
                                 (lista-replace pop-mutacao posicao-mutacao individuo-novo))
                                ((< valor taxa-mutacao)
                                 (lista-replace pop-mutacao posicao-mutacao
                                                (formula-random-grow operadores #:max gp-valores)))
                                (else pop-mutacao)
                                ))
                    (populacao-ordena
                     (append individuos-manter pop-mutacao) input-lista output-lista)
                    )
                ))
            ))
        )
      ))
  )

(define (gp-run input-lista output-lista [operadores *operadores*]
                #:fd [fd-arq (current-output-port)]  #:gp-valor [gp-valores gp-padrao])
  (let* ((populacao-inicial     (populacao-criar-inicial operadores))
         (populacao-inicial-ord (populacao-ordena populacao-inicial input-lista output-lista)))
    (let loop ((repete 0) (melhor-fitness 0) (pop-aux populacao-inicial-ord))
      (let ((nova-geracao (populacao-proxima-geracao pop-aux input-lista output-lista
                                                     operadores #:gp-valor gp-valores)))
        (displayln (~a "Best fitness: "
              (individuo-parametros-fitness (first nova-geracao))) fd-arq)
        (let ((ind-fitness (individuo-parametros-fitness (first nova-geracao)))
              (ind-formula (individuo-parametros-formula (first nova-geracao)))
              (fitness-min (gp-finalizar-fitness-min gp-fim))
              (repeticoes  (gp-finalizar-repeticoes gp-fim)))
          (if (or (> ind-fitness fitness-min)
                  (> repete repeticoes))
              (first nova-geracao)
              (if (= ind-fitness melhor-fitness)
                  (loop (+ repete 1) melhor-fitness nova-geracao)
                  (loop 0 ind-fitness nova-geracao))
              )
          )
        )
      )
    ))

(define (funcao-lista min max step funcao)
  (values
  (for/list ((i (in-range min (+ max step) step)))
    i) 
  (for/list ((i (in-range min (+ max step) step)))
    (funcall funcao i)))
  )

(define (funcao-entrada min max step funcao [operadores *operadores*]
                        #:fd [fd-arq (current-output-port)] #:gp-valor [gp-valores gp-padrao])
  (let-values (((input-lista output-lista) (funcao-lista min max step funcao)))
    (gp-parametros-display gp-valores fd-arq)
    (gp-run input-lista output-lista operadores #:fd fd-arq #:gp-valor gp-valores))
  )

(define (gp-run-funcao nome-dir min max step funcao [operadores *operadores*]
                       #:gp-valor [gp-valores gp-padrao])
  (let-values (((input-lista output-lista) (funcao-lista min max step funcao)))
    (let* ((dir-trabalho (string-append *dir-output* nome-dir))
           (grafico (string-append dir-trabalho "/" "grafico.png")))
      (cond ((not (directory-exists? dir-trabalho))
             (make-directory dir-trabalho)
             (copy-file (string-append *dir-output* "gp.xsl")
                        (string-append dir-trabalho "/gp.xsl"))))
      (let* ((individuo-resposta
             (call-with-output-file #:mode 'text #:exists 'replace
               (string-append dir-trabalho "/" "log")
               (lambda(p)
                 (gp-parametros-display gp-valores p)
                 (gp-run input-lista output-lista operadores #:fd p #:gp-valor gp-valores)
                 )))
             (formula-resposta (individuo-parametros-formula individuo-resposta)))
        ;;individuo-resposta
        (lambda (#:point [ponto #f] #:min-plot [min-grafico #f] #:max-plot [max-grafico #f])
          (cond [(number? ponto)
                 (formula-run formula-resposta ponto)]
                [(and (number? min-grafico) (number? max-grafico))
                 (plot (function (lambda(x) (formula-run formula-resposta x))
                                 min-grafico max-grafico))]
                [else
                 (call-with-output-file #:mode 'text #:exists 'replace
                   (string-append dir-trabalho "/" "gp.xml")
                   (lambda(p)
                     (inicio-display p)
                     (gp-parametros-display gp-valores p)
                     (individuo-display individuo-resposta p)
                     (tabela-display input-lista output-lista p)
                     (grafico-display "grafico.png" p)
                     (grafico-plot input-lista output-lista
                                   formula-resposta grafico)
                     (fim-display p)
                     ))
                 ])
          )
      ))
    )
  )

(define (gp-arq nome-dir [operadores *operadores*]
                #:gp-valor [gp-valores gp-padrao])
  (let* ((dir-trabalho (string-append *dir-output* nome-dir))
         (grafico (string-append dir-trabalho "/" "grafico.png")))
    (cond ((and (directory-exists? dir-trabalho) (not (file-exists? (string-append dir-trabalho "/gp.xsl"))))
           (copy-file (string-append *dir-output* "gp.xsl")
                      (string-append dir-trabalho "/gp.xsl"))))
    (let ((input-output
           (call-with-input-file (string-append dir-trabalho "/input")
             (lambda(p)
               (read p)))))
      (let ((input-lista  (car input-output))
            (output-lista (cadr input-output)))
        (let* ((individuo-resposta
                (call-with-output-file #:mode 'text #:exists 'replace
                  (string-append dir-trabalho "/" "log")
                  (lambda(p)
                    (gp-parametros-display gp-valores p)
                    (gp-run input-lista output-lista operadores #:fd p #:gp-valor gp-valores)
                    )))
               (formula-resposta (individuo-parametros-formula individuo-resposta)))
          (lambda (#:point [ponto #f] #:min-plot [min-grafico #f] #:max-plot [max-grafico #f])
            (cond [(number? ponto)
                   (formula-run formula-resposta ponto)]
                  [(and (number? min-grafico) (number? max-grafico))
                   (plot (function (lambda(x) (formula-run formula-resposta x))
                                   min-grafico max-grafico))]
                  [else
                   (call-with-output-file #:mode 'text #:exists 'replace
                     (string-append dir-trabalho "/" "gp.xml")
                     (lambda(p)
                       (inicio-display p)
                       (gp-parametros-display gp-valores p)
                       (individuo-display individuo-resposta p)
                       (tabela-display input-lista output-lista p)
                       (grafico-display "grafico.png" p)
                       (grafico-plot input-lista output-lista
                                     formula-resposta grafico)
                       (fim-display p)
                       ))
                   ])
            )
          ))
      ))
  )

(define (grafico-plot input-lista output-lista formula nome-grafico #:file? [plot-file? #t])
  (let ((entrada (points
                 (for/list ((input  (in-list input-lista))
                            (output (in-list output-lista)))
                   (list input output)
                   )
                 #:alpha 1
                 #:sym 'fullcircle1
                 #:color "blue"
                 #:label "target"))
        (obtido (points
                 (for/list ((input (in-list input-lista)))
                   (list input (formula-run formula input)))
                #:alpha 1
                #:sym 'fullcircle1
                #:color "red"
                #:label "output"))
        )
    (if plot-file?
        (plot-file (list entrada obtido) nome-grafico
          #:x-min (- (car (sort input-lista <)) 1)
          #:x-max (+ (car (sort input-lista >)) 1)
          #:y-min (- (car (sort output-lista <)) 1)
          #:y-max (+ (car (sort output-lista >)) 1))

        (plot (list entrada obtido)
          #:x-min (- (car (sort input-lista <)) 1)
          #:x-max (+ (car (sort input-lista >)) 1)
          #:y-min (- (car (sort output-lista <)) 1)
          #:y-max (+ (car (sort output-lista >)) 1))
        )
    )
  )

(define (gp-parametros-display gp-valores p)
  (let ((max    (gp-parametros-max-populacao  gp-valores))
        (taxa   (gp-parametros-taxa-mutacao   gp-valores))
        (chance (gp-parametros-chance-mutacao gp-valores))
        (depth  (gp-parametros-max-depth      gp-valores)))
    (displayln (~a "<max-populacao>") p)
    (displayln (~a max ) p)
    (displayln (~a "</max-populacao>") p)
    (displayln (~a "<taxa-mutacao>") p)
    (displayln (~a taxa) p)
    (displayln (~a "</taxa-mutacao>") p)
    (displayln (~a "<chance-mutacao>") p)
    (displayln (~a chance) p)
    (displayln (~a "</chance-mutacao>") p)
    (displayln (~a "<max-depth>") p)
    (displayln (~a depth) p)
    (displayln (~a "</max-depth>") p)
    ))

(define (individuo-display ind p)
  (let ((formula (individuo-parametros-formula ind))
        (fitness (individuo-parametros-fitness ind)))
    (displayln (~a "<formula>") p)
    (displayln (~a formula) p)
    (displayln (~a "</formula>") p)
    (displayln (~a "<fitness>") p)
    (displayln (~a fitness) p)
    (displayln (~a "</fitness>") p)
    )
  )

(define (grafico-display nome p)
  (displayln (~a "<grafico>") p)
  (displayln (~a nome) p)
  (displayln (~a "</grafico>") p))

(define (tabela-display input-lista output-lista p)
  (displayln (~a "<tabela>") p)
  (for ((input  (in-list input-lista))
        (output (in-list output-lista)))
    (displayln (~a "<valores>") p)
    (displayln (~a "<input>") p)
    (displayln (~a input) p)
    (displayln (~a "</input>") p)
    (displayln (~a "<output>") p)
    (displayln (~a output) p)
    (displayln (~a "</output>") p)
    (displayln (~a "</valores>") p)
    )
  (displayln (~a "</tabela>") p)
  )

(define (inicio-display p)
  (displayln (~a "<?xml version='1.0' encoding='UTF-8'?>                                                                                                           
<?xml-stylesheet type='text/xsl' href='gp.xsl'?>                                                                                                 
<gp>  ") p))

(define (fim-display p)
  (displayln (~a "</gp>") p))

(define (populacao-display populacao)
  (for ((ind (in-list populacao)))
    (displayln (~a (individuo-parametros-formula ind) "    "
                   (individuo-parametros-fitness ind)))
    ))
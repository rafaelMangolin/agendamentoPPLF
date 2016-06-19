#lang racket

;; Este programa encontra horários disponíveis que sejam comuns entre vários
;; horários especificados e que tenham um tamanho mínimo especificado.
;;
;; ** Conceitos **
;;  Horário
;;    Um momento no tempo, definido em termos da hora e minutos
;;  Intervalo (abreviado inter)
;;    Um intervalo no tempo, tem um horário de início e um horário de fim
;;  Disponibilidade do dia (abreviado dispo)
;;    Uma lista de intervalos que estão disponíveis em um determinado dia
;;  Disponibilidade semanal (abreviado dispo-semana)
;;    Uma lista com as disponibilidades de cada dia
;;  Lista de associações
;;    Uma lista de pares. Um par é uma lista com dois elementos. O primeiro
;;    elemento do par é chamado de chave e o segundo elemento é chamado de
;;    valor. Uma lista de associações é uma maneira simples de implementar uma
;;    tabela associativa (dicionário).  Ex: o dicionário
;;    1 -> 4, 20 -> 12, 6 -> 70, pode ser representado pela lista associativa
;;    (list (list 1 4) (list 20 12) (list 6 70)).
;;    A função assoc é utilizada para consultar uma lista associativa.
;;
;; ** Formatação de entrada e saída **
;; Toda operação de entrada e saída deve ser feita respeitando essas
;; formatações. A sua implementação não precisa validar as entradas. Para os
;; testes automatizados as entradas sempre serão válidas.
;;
;;  Horário (HH:MM) (sempre 5 dígitos)
;;  Exemplos
;;     08:30 =  8 horas e 30 minutos
;;     12:07 = 12 horas e  7 minutos
;;
;;  Intervalo (HH:MM-HH:MM) (sempre 11 dígitos)
;;  Exemplos
;;     08:30-12:07 = o intervalo tem início às 8 horas e 30 minutos e tem
;;                   o fim às 12 horas e 7 minutos
;;
;;  Dias da semana
;;    Representados por strings de tamanho 3: dom seg ter qua qui sex sab
;;
;;  Disponibilidade semanal
;;    Uma sequência de linhas. Cada linha contém o dia e a lista de
;;    intervalos disponíveis naquele dia
;;  Exemplo
;;    ter 10:20-12:00 16:10-17:30
;;    sex 08:30-11:30
;;  Observe que nem todos os dias devem estar especificados. Os dias
;;  que não têm disponibilidades não devem ser especificados.


;; exporta as funções que podem ser utilizadas em outros arquivos
(provide horario
         intervalo
         intervalo-vazio
         intervalo-vazio?
         intervalo-intersecao
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)

(struct horario (h m) #:transparent)
;; Horário representa um momento no tempo, definido em termos da hora e minutos
;;    h : Número - horas
;;    m : Número - minutos

(struct intervalo (inicio fim) #:transparent)
;; Intervalo representa um intervalo no tempo, tem um horário de início e um
;; horário de fim
;;    inicio : Horário - horário de início
;;       fim : Horário - horário de fim

;; Constante que define um intervalo vazio
(define intervalo-vazio (void))

;; Intervalo -> bool
;; Retorna #t se inter representa o intervalo vazio, #f caso contrário
(define (intervalo-vazio? inter)
  (equal? inter intervalo-vazio))

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (let* ([maior-inicio (maior-horario (intervalo-inicio a) (intervalo-inicio b))]
        [menor-fim (menor-horario (intervalo-fim a) (intervalo-fim b))]
        [intervalo-intersecao (intervalo maior-inicio menor-fim)])
  (cond
    [(intervalo-vazio? a) intervalo-vazio]
    [(intervalo-vazio? b) intervalo-vazio]
    [(equal? a b) a]
    [(intervalo-valido? intervalo-intersecao) intervalo-intersecao]
    [else intervalo-vazio])))

;; Horario, Horario -> Horario
;; Funcao responsavel por retornar o horario com o maior termino.
(define (maior-horario h1 h2)
  (cond
    [(not (horario? h1)) (error "Deve ser passado uma estrutura horario na 1 posição")]
    [(not (horario? h2)) (error "Deve ser passado uma estrutura horario na 2 posição")]
    [(> (horario-h h1) (horario-h h2)) h1]
    [(> (horario-h h2) (horario-h h1)) h2]
    [(> (horario-m h1) (horario-m h2)) h1]
    [else h2]))

;; Horario, Horario -> Horario
;; Funcao responsavel por retornar o horario com o menor termino.
(define (menor-horario h1 h2)
  (cond
    [(not (horario? h1)) (error "Deve ser passado uma estrutura horario na 1 posição")]
    [(not (horario? h2)) (error "Deve ser passado uma estrutura horario na 2 posição")]
    [(< (horario-h h1) (horario-h h2)) h1]
    [(< (horario-h h2) (horario-h h1)) h2]
    [(< (horario-m h1) (horario-m h2)) h1]
    [else h2]))

;; Intervalo -> Boolean
;; Funcao responsavel por retornar #t se um Intervalo e valido, e #f caso contrario.
;; Considera-se como intervalo valido caso o Segundo Horario for maior que o Primeiro Horario.
(define (intervalo-valido? inter)
  (equal? (intervalo-fim inter) (maior-horario (intervalo-fim inter) (intervalo-inicio inter))))

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (cond
    [(empty? dispo-a) empty]
    [(empty? dispo-b) empty]
    [else (append
           (encontra-intersecao-intervalo-lista (first dispo-a) dispo-b)
           (encontrar-dispo-em-comum (rest dispo-a) dispo-b))]))

;; Intervalo , list Intervalo -> list Intervalo
;; Encontra as intercecoes do intervalo com a lista
(define (encontra-intersecao-intervalo-lista inter dispo)
  (cond
    [(empty? dispo) empty]
    [(intervalo-vazio? inter) empty]
    [(intervalo-vazio? (intervalo-intersecao inter (first dispo))) (encontra-intersecao-intervalo-lista inter (rest dispo))]
    [else (cons (intervalo-intersecao inter (first dispo)) (encontra-intersecao-intervalo-lista inter (rest dispo)))]
   ))

;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra os intervalos disponíveis para cada dia da semana que
;; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
;; da lista dispos.
;;
;; dispo-semana é uma lista de associações entre um dia (string) e a
;; disponibilidade naquele dia. Veja a definição de lista de associações no
;; início deste arquivo.
;;
;; Por exemplo, a disponibilidade semanal (dispo-semana):
;; ter 10:20-12:00 16:10-17:30
;; sex 08:30-11:30
;; é representada da seguinte maneira:
;; (list (list "ter" (list (intervalo (hora 10 20) (hora 12 00))
;;                         (intervalo (hora 16 10) (hora 17 30))))
;;       (list "sex" (list (intervalo (hora 08 30) (hora 11 30)))))
;;
;; Observe que esta função recebe como parâmetro uma lista de disponibilidades
;; semanais, o exemplo acima refere-se a apenas uma disponibilidade semanal.
;; Veja os testes de unidade para exemplos de entrada e saída desta função
(define (encontrar-dispo-semana-em-comum tempo dispos)
  (filter dia-com-dispo? (dispo-com-intervalos-filtrados-por-tempo (foldl list-dispo->dispo (first dispos) (rest dispos)) (get-minutos tempo))))

;; List (String List) -> Boolean
;; Verifica se o dia possui intervalos
(define (dia-com-dispo? dia)
  (cond
    [(empty? (first (rest dia))) #f]
    [else #t]))

;; List intervalo, Number -> List intervalo 
;(define (intervalo-valido-por-tempo? intervalo)
 ;   (>= (diferenca-horas (first list)) tempo))

;; Dispo, Number -> Dispo
;; Retorna a dispo com apenas os intervalos que sejam validos
;; ao tempo passado.
(define (dispo-com-intervalos-filtrados-por-tempo dispo tempo)
  (map (λ(dia)
         (cons (first dia) (cons
                (filter (λ(intervalo) (>= (diferenca-horas intervalo) tempo)) (first(rest dia)))
                empty))) dispo))

;; String, String -> Boolean
;; Verifica se a string do dia a é maior que do b
(define (dia-a-maior-que-b? a b)
  (cond
    [(equal? a b) #f]
    [(equal? a "dom") #f]
    [(equal? b "dom") #t]
    [(equal? a "seg") #f]
    [(equal? b "seg") #t]
    [(equal? a "ter") #f]
    [(equal? b "ter") #t]
    [(equal? a "qua") #f]
    [(equal? b "qua") #t]
    [(equal? a "qui") #f]
    [(equal? b "qui") #t]
    [(equal? a "sex") #f]
    [(equal? b "sex") #t]
    [else #f]))

;; Dispo, Dispo -> Dispo
;; Funçao usada no fold para fazer a intercesao da lista de dispo
(define (list-dispo->dispo base atual)
  (cond
    [(empty? atual) base]
    [(equal? base void) base]
    [else (encontra-inter-entre-dispo base atual)]
    ))

;; Dispo, Dispo -> Dispo
;; Função responsavel por montar a intercesao entre dois dispo, 
(define (encontra-inter-entre-dispo a b)
 (cond
    [(empty? a) empty]
    [(empty? b) empty]
    [(equal? (first (first a)) (first (first b)))
     (append
      (cons (cons (first (first a)) (cons (encontrar-dispo-em-comum (first(rest (first a))) (first (rest (first b))) ) empty)) empty)
      (encontra-inter-entre-dispo (rest a) (rest b)))]
    [(dia-a-maior-que-b? (first (first a)) (first (first b))) (encontra-inter-entre-dispo a (rest b))]
    [else (encontra-inter-entre-dispo (rest a) b)]
  ))

;; Horario -> Natural
;; Funcao responsavel por retornar um Horario em Minutos.
;; Exemplos:
;;     (get-minutos (horario 0 0))   ->   0 
;;     (get-minutos (horario 0 43))  ->  43
;;     (get-minutos (horario 1 43))  -> 103
;;     (get-minutos (horario 10 30)) -> 630
(define (get-minutos h1)
  (+ (* (horario-h h1) 60)
     (horario-m h1)))

;; Intervalo -> Natural
;; Funcao responsavel por retornar a diferenca (em minutos) do Horario Inicial para o Horario Final.
;; Exemplos:
;;     (diferenca-horas (intervalo (horario 15 10) (horario 15 30))) ->  20
;;     (diferenca-horas (intervalo (horario 15 30) (horario 15 30))) ->   0
;;     (diferenca-horas (intervalo (horario 15 40) (horario 15 30))) -> -10
;;     (diferenca-horas (intervalo (horario 15 10) (horario 16 30))) ->  80
;;     (diferenca-horas (intervalo (horario 15 10) (horario 19 10))) -> 240
(define (diferenca-horas inter)
  (- (get-minutos (intervalo-fim    inter))
     (get-minutos (intervalo-inicio inter))))

;; Natural -> Horario
;; Funcao responsavel por retornar um Horario pelo numero de minutos.
;; Exemplos:
;;     (converte-horario  0)  -> void
;;     (converte-horario 10)  -> (horario 0 10)
;;     (converte-horario 60)  -> (horario 1 0)
;;     (converte-horario 130) -> (horario 2 10)
(define (converte-horario n)
  (cond
    [(<= n 0) void]
    [else (horario (divisao-inteira n 60) (remainder n 60))]))

;; Natural, Natural -> Natural
;; Funcao responsavel por retornar a parte inteira da Divisao.
;; Exemplos:
;;     (divisao-inteira 2 0)    -> 0
;;     (divisao-inteira 2 1)    -> 2
;;     (divisao-inteira 2 3)    -> 0
;;     (divisao-inteira 100 40) -> 2
(define (divisao-inteira n d)
  (cond
    [(zero? d) 0]
    [(< n d) 0]
    [else (+ 1 (divisao-inteira (- n d) d))]))

;; Horario -> Boolean
;; Funcao responsavel por retornar true se o horario e vazio, e false caso contrario.
;; Exemplos:
;;     (horario-vazio? (horario 0 0)) -> #t
;;     (horario-vazio? (horario 0 1)) -> #f
(define (horario-vazio? horario)
  (cond
    [(void? horario) #t]
    [(> (horario-h horario) 0) #f]
    [else (= (horario-m horario) 0)]))

;; list string -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; reuni-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro parâmetro é o tempo mínimo (string) que os intervalos em comum
;; devem ter. O tempo mínimo é especificado usando a formatação de horário.
;;
;; O restante dos parâmetros são nomes de arquivos. Cada arquivo de entrada
;; contêm uma disponibilidade semanal. Veja exemplos de arquivos no diretórios
;; testes.
;;
;; A saída desta função é a escrita na tela dos intervalos em comum que
;; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
;; semanal.
(define (main args)
  (printf (dispo->string (encontrar-dispo-semana-em-comum (string->horario (first args)) (list-arquivo->list-dispo (rest args))))))

;; List string -> List dispo
;; Função responsavel por converter uma lista de string (contendo o endereço dos arquivos)
;; em uma lista de dispos.
(define (list-arquivo->list-dispo list)
  (cond
    [(empty? list) empty]
    [else (cons (arquivo->dispo (open-input-file (first list))) (list-arquivo->list-dispo (rest list)))]))

;; String -> Horario
;; Função responsavel em converter uma String no formato ("09:30")
;; para Horario
(define (string->horario s)
  (let ([string-list (string-split s ":")])
  (cond
    [(= 1 (length string-list)) void]
    [else
     (horario
      (string->number (first string-list))
      (string->number (first (rest string-list))))])))

;; String -> Intervalo
;; Função responsavel em converter string no formato ("10:30-11:20")
;; para Intervalo
(define (string->intervalo s)
  (let ([string-list (string-split s "-")])
  (intervalo (string->horario (first string-list)) (string->horario (first (rest string-list))))))

;; List String -> Dia
;; Função responsavel em converter um lista de String em Dia formatado como
;; '("seg" '((intervalo (horario 1 2) (horario 2 3))))
(define (list-string->dia dia)
  (cond
    [(empty? dia) empty]
    [(= 3 (string-length (first dia))) (cons (first dia) (cons (list-string->dia (rest dia)) empty))]
    [else (cons (string->intervalo (first dia)) (list-string->dia (rest dia)))]))

;; Ponteiro -> Dispo
;; Função responsavel em ler o arquivo referenciado no ponteiro com o conteudo:
;; seg 10:10-11:00
;; ter 12:10-14:00
;; e transformalo em dispo
(define (arquivo->dispo ponteiro)
  (let ([linha (read-line ponteiro)])
  (cond
    [(eof-object? linha) empty]
    [else (cons (list-string->dia (string-split linha " ")) (arquivo->dispo ponteiro))])))

;; Dispo -> String
;; Função responsavel por converter um dispo para string no formato
;; seg 10:10-11:00
;; ter 12:10-14:00
(define (dispo->string dispo)
 (string-join (map dia->string dispo) ""))

;; Dia -> String
;; Função responsavel por converter o dia para string no formato:
;; seg 10:10-11:00 11:20-16:00
(define (dia->string dia)
  (string-append (first dia) " " (string-join (map intervalo->string (first (rest dia))) " ") "~%"))

;; Intervalo -> String
;; Função responsável por converterum intervalo para string no formato:
;; 10:10-11:00
(define (intervalo->string intervalo)
  (string-join (list (horario->string (intervalo-inicio intervalo)) (horario->string (intervalo-fim intervalo))) "-"))

;; Horario -> String
;; Função responsavel por converter um Horario para String no formato:
;; 10:10
(define (horario->string horario)
  (string-append (int->string-min-dois-digitos (horario-h horario)) ":" (int->string-min-dois-digitos (horario-m horario))))

;; Number -> String
;; Função responsavel por retornar uma string de um numero com no minimo dois digitos.
;; Exemplo:
;; 1 -> "01"
;; 10 -> "10"
(define (int->string-min-dois-digitos num)
  (cond
    [(< num 10) (string-append "0" (number->string num))]
    [else (number->string num)]))

(main (list "00:01" "../testes/c" "../testes/b" "../testes/livre"))
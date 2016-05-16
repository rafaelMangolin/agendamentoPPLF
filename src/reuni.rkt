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
  (error "Não implementado"))

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (error "Não implementado"))

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
  (error "Não implementado"))

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
  (error "Não implementado"))

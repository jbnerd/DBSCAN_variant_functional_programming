#lang racket

(define (truncate_list ls D)
  (cond
    ((= D 0) ls)
    ((= D 1) (cdr ls))
    (else (truncate_list (cdr ls) (- D 1)))  
  )
)
;(truncate_list '(0 1 2 3 4 5) 0)

(define (next_D ls D)
  (if (= D 0)
    '()
    (append (list (car ls)) (next_D (cdr ls) (- D 1)))
  )
)
;(next_D '(0 1 2 3 4 5) 3)

(define (get_inp ls D)
  (cond
    ((null? ls) '())
    (else (cons (next_D ls D) (get_inp (truncate_list ls D) D)))
  )
)

(define (add_index ls idx)
  (if (null? ls) '()
    (cons (append (list idx) (list (car ls))) (add_index (cdr ls) (+ idx 1)))
  )
)

;(define temp_list (get_inp '(0 1 2 3 4 5 6 7 8 9 10 11) 4))
;temp_list
;(add_index temp_list 0)

(define file_list (file->list "./t0.in"))
;file_list
(define N (list-ref file_list 0))
(define D (list-ref file_list 1))
(define K (list-ref file_list 2))
(define eps (list-ref file_list 3))
(define MinPts (list-ref file_list 4))
(define raw_inp_list (truncate_list file_list 5))
(define inp_list (get_inp raw_inp_list D))
(define step1 (add_index inp_list 1))
step1
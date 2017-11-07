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

(define (sq_dist ls1 ls2)
  (if (and (null? ls1) (null? ls2)) 0
    (+ (expt (- (car ls1) (car ls2)) 2) (sq_dist (cdr ls1) (cdr ls2)))
  )
)

(define (similarity ls1 ls2)
  (sqrt (sq_dist ls1 ls2))
)
;(similarity '(1 1 1) '(2 2 2))

(define (point_eq point1 point2)
  (cond
    ((and (null? point1) (null? point2)) #t)
    ((not (= (car point1) (car point2))) #f)
    (else (point_eq (cdr point1) (cdr point2))) 
  )
)
;(point_eq '(0 0) '(0 0))
;(point_eq (cdr (list '(1 (0 0)))) (cdr (list '(1 (0 0)))))

(define (find_distances point point_list idx)
  (cond
    ((null? point_list) '())
    ((point_eq point (car point_list)) (cons (append (list idx) (list +inf.0)) (find_distances point (cdr point_list) (+ idx 1)))) 
    (else (cons (append (list idx) (list (similarity point (car point_list)))) (find_distances point (cdr point_list) (+ idx 1))))
  )
)
;(find_distances '(0 0) '((0 0) (0 1) (1 0) (1 1)) 1)

(define (sim_matrix point_list idx len)
  (if (= idx len) '()
      (cons (find_distances (list-ref point_list idx) point_list 1) (sim_matrix point_list (+ idx 1) len))
  )
)
;(sim_matrix '((0 1) (1 0) (1 1)) 0 3)

(define (del_indices point_list)
  (if (null? point_list) '()
    (append (cdr (car point_list)) (del_indices (cdr point_list)))
  )
)


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
;step1
;(define temp (del_indices step1))
;temp
(define step2 (sim_matrix (del_indices step1) 0 N))
;step2
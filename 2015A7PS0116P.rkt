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

(define (sort_by_dist ls)
  (sort ls
    (lambda (x y) (<= (list-ref x 1) (list-ref y 1)))
  )
)
;(define temp '((1 48.42207244635446) (2 63.40318525121589) (3 9.005415037631531) (4 11.510139008717486) (5 47.76110970235093) (6 33.486607770868645) (7 1.105983725015878) (8 54.19638456576232) (9 12.289548405047272) (10 68.17098429683996) (11 71.67660636497797) (12 55.7953940751385) (13 63.247174640453316) (14 47.67632116680145) (15 73.54982256946649) (16 8.68386434716711) (17 36.75840176068595) (18 65.23667450138764) (19 10.093854566021836) (20 +inf.0)))
;(sort_by_dist temp)

(define (sort_matrix matrix)
  (if (null? matrix) '()
    (cons (sort_by_dist (car matrix)) (sort_matrix (cdr matrix)))
  )
)

(define (knn ls K)
  (if (= K 0) '()
    (append (list (car (car ls))) (knn (cdr ls) (- K 1)))
  )
)
;(define temp '((1 48.42207244635446) (2 63.40318525121589) (3 9.005415037631531) (4 11.510139008717486) (5 47.76110970235093) (6 33.486607770868645) (7 1.105983725015878) (8 54.19638456576232) (9 12.289548405047272) (10 68.17098429683996) (11 71.67660636497797) (12 55.7953940751385) (13 63.247174640453316) (14 47.67632116680145) (15 73.54982256946649) (16 8.68386434716711) (17 36.75840176068595) (18 65.23667450138764) (19 10.093854566021836) (20 +inf.0)))
;(knn temp 4)

(define (knn_matrix matrix K)
  (if (null? matrix) '()
    (cons (knn (car matrix) K) (knn_matrix (cdr matrix) K))
  )
)

(define (weight knn1 knn2)
  (if (null? knn1) '()
    (if (member (car knn1) knn2)
      (cons (car knn1) (weight (cdr knn1) knn2))
      (weight (cdr knn1) knn2)
    )
  )
)
;(weight '(1 2 3 4 7) '(3 4 5 6))

(define (create_edges knn_list knn_matrix)
  (if (null? knn_list) '()
    (cons (cons (car knn_list) (weight knn_list (list-ref knn_matrix (car knn_list)))) (create_edges (cdr knn_list) knn_matrix))
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
(define step2 (sim_matrix (del_indices step1) 0 N))
;step2
;(sort_matrix step2)
(define step3 (knn_matrix (sort_matrix step2) K))
step3
;(create_edges (car step3) step3)
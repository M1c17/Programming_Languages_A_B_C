#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define ones (lambda () (cons 1 ones)))

;; Problem (1)

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem (2)

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

;; Problem 3

(define get-nth
  (lambda (xs n)
    (cond [(null? xs) '()]
          [(= 0 n) (car xs)]
          [else (get-nth (cdr xs)(- n 1))])))

(define (list-nth-mod xs n)
  (cond [(negative? n)(error "list-nth-mod: negative number")]
        [(null? xs)(error "list-nth-mod: empty list")]
        [#t (get-nth xs (remainder n (length xs)))]))

;; Problem 4

(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s))(stream-for-n-steps (cdr (s)) (- n 1)))))

;; Problem 5

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- 0 x) x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem 6

(define dan-then-dog
  (letrec ([next_dan (lambda () (cons "dan.jpg" (lambda () (next_dog))))]
           [next_dog (lambda () (cons "dog.jpg" (lambda () (next_dan))))])
    (lambda () (next_dan))))

;; Problem 7

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s)))
                   (lambda () ((stream-add-zero (cdr (s))))))))

;; Problem 8

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([x (list-nth-mod xs n)]
                      [y (list-nth-mod ys n)])
                (cons (cons (x y))(lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

;; Problem 9

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(>= n (vector-length vec)) #f]
                      [(and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v)) (vector-ref vec n)]
                      [#t (f (+ n 1))]))])
    (f 0)))

;; Problem 10

;; return a function that takes one argument v & returns the same
;; thing that (assoc v xs) would return
;; The cache must be a Racket vector of length n

(define (cached-assoc xs n)
  (letrec ([pos 0]
           [memo-cache (make-vector n #f)])
    (lambda (v)
      (letrec ([search_cache (vector-assoc v memo-cache)])
        (if search_cache search_cache
            (letrec ([new-ans (assoc v xs)])
              (if new-ans
                  (begin
                    (vector-set! memo-cache pos new-ans)
                    (if (= pos (- n 1))
                        (set! pos 0)
                        (set! pos (+ pos 1)))
                    new-ans)
                  #f)))))))

;; Challenge Problem

(define-syntax while-less 
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([loop (lambda ()
                      (if (>= e2 e1)
                          #t
                          (loop)))])
       (loop))]))

;-----------------------------------------------------------

(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;"x""x""x""x""x"#t
;set! -> change a to be 7 
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;"x"#t
;set! -> change a to be 8

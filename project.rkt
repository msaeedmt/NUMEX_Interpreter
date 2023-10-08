;; PL Project - Fall 2020
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool  (boolean)    #:transparent)

(struct neg (e)  #:transparent)

(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)
(struct mult  (e1 e2)  #:transparent)
(struct div  (e1 e2)  #:transparent)

(struct andalso (e1 e2) #:transparent)
(struct orelse (e1 e2)  #:transparent)

(struct cnd (e1 e2 e3)  #:transparent)
(struct ifnzero (e1 e2 e3)  #:transparent)
(struct iseq (e1 e2)  #:transparent)
(struct ifleq (e1 e2 e3 e4)  #:transparent)
(struct with (s e1 e2)  #:transparent)

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application
(struct appli (funexp actual)       #:transparent) ;; function application

(struct apair (e1 e2) #:transparent)
(struct 1st (e) #:transparent)
(struct 2nd (e) #:transparent)

(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false
(struct isapair (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions





;; Problem 1


(define (racketlist->numexlist xs) (cond ((null? xs) (munit))
                                         (true (apair (car xs) (racketlist->numexlist (cdr xs))))))
                                         ;(true (cond [(integer? (car xs)) (apair (num (car xs)) (racketlist->numexlist (cdr xs)))]
                                              ;       [(boolean? (car xs)) (apair (bool (car xs)) (racketlist->numexlist (cdr xs)))]))))
                                                     
                            
(define (numexlist->racketlist xs) (cond ((munit? xs) '())
                                         (true (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs))))))
                                         ;(true (cond [(num? (apair-e1 xs)) (cons (num-int (apair-e1 xs)) (numexlist->racketlist (apair-e2 xs)))]
                                         ;            [(bool? (apair-e1 xs)) (cons (bool-boolean (apair-e1 xs)) (numexlist->racketlist (apair-e2 xs)))]))))
                                         

(define (convert xs) (cond [(equal? xs (munit)) '()]
                                         [(num? (apair-e1 xs)) (cons (num-int (apair-e1 xs))(convert (apair-e2 xs)))]
                                         [(bool? (apair-e1 xs)) (cons (bool-boolean (apair-e1 xs))(convert (apair-e2 xs)))]
                                        ))
(define (salam) (car (cons 'x (num 3))))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))


(define (eval-under-env e env)
  (cond [(var? e)
         (let ([v (envlookup env (var-string e))])
           (cond [(or (num? v) (bool? v)) v]
                 [true (eval-under-env v env)]))]
        [(num? e) 
         (cond [(integer? (num-int e)) e]
               [#t (error "NUMEX num must contain an integer")])]
        [(bool? e)
         (cond [(boolean? (bool-boolean e)) e]
               [#t (error "NUMEX bool must contain a boolean")])]
        [(munit? e)
         (munit)]
        [(string? e) e]
        [(key? e) e]
        [(closure? e) e]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient  (num-int v1) 
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]
        [(andalso? e)
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           (if (and (bool? v1) (equal? (bool-boolean v1) #f)) v1
               (let ([v2 (eval-under-env (andalso-e2 e) env)])
                 (if (bool? v2) v2
                     (error "NUMEX andalso applied to non-bool")))))]

        [(orelse? e)
         (let ([v1 (eval-under-env (orelse-e1 e) env)])
           (if (and (bool? v1) (eq? (bool-boolean v1) #t)) v1
               (let ([v2 (eval-under-env (orelse-e2 e) env)])
                 (if (bool? v2) v2
                     (error "NUMEX andalso applied to non-bool")))))]
        [(neg? e)
         (let ([v (eval-under-env (neg-e e) env)])
           (cond [(num? v) (num (- (num-int v)))]
                 [(bool? v) (cond ((equal? true (bool-boolean v)) (bool #f)) (true (bool #t)))]
                 [true (error "NUMEX negative applied to non-proper value")]))]
        [(cnd? e)
         (let ([v (eval-under-env (cnd-e1 e) env)])
           (if (bool? v) 
               (cond [(equal? (bool-boolean v) true) (eval-under-env (cnd-e2 e) env)]
                 [true (eval-under-env (cnd-e3 e) env)])
               (error "NUMEX cnd applied to a non-boolean condition")))]
        [(iseq? e)
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (cond [(and (bool? v1) (bool? v2)) (cond [(equal? (bool-boolean v1) (bool-boolean v2)) (bool #t)]
                                                    [true (bool #f)])]
                 [(and (num? v1) (num? v2)) (cond [(equal? (num-int v1) (num-int v2)) (bool #t)]
                                                  [true (bool #f)])]
                 [true (bool #f)]))]
        [(ifnzero? e)
         (let ([v (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v) 
               (cond [(equal? (num-int v) 0) (eval-under-env (ifnzero-e3 e) env)]
                     [true (eval-under-env (ifnzero-e2 e) env)])
               (error "NUMEX ifnzero applied to a non-number condition")))]
        [(ifleq? e)
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (cond [(> (num-int v1) (num-int v2)) (eval-under-env (ifleq-e4 e) env)]
                     [true (eval-under-env (ifleq-e3 e) env)])
               (error "NUMEX ifleq applied to non-number")))]
        [(with? e)
         (let ([v1 (eval-under-env (with-e1 e) env)])
           (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env)))]
        
        [(ismunit? e) 
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (if (munit? v1) (bool #t)
               (bool #f)))]

        [(lam? e)
         (cond [(null? (lam-nameopt e)) (closure env e)]
               [true (closure (cons (cons (lam-nameopt e) e) env) e)])]

        [(apply? e)
         (let ([v (eval-under-env (apply-funexp e) env)]
               [arg (eval-under-env (apply-actual e) env)])
           (cond [(closure? v) (eval-under-env (lam-body (closure-f v)) (cons (cons (lam-formal (closure-f v)) arg) (closure-env v)))]
                 [true (error "first argument should return a closure")]))]

        
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(1st? e)
         (let ([v (eval-under-env (1st-e e) env)])
           (cond [(apair? v) (apair-e1 v)]
                 [true (error "NUMEX 1st applied to non-apair")]))]
        [(2nd? e)
         (let ([v (eval-under-env (2nd-e e) env)])
           (cond [(apair? v) (apair-e2 v)]
                 [true (error "NUMEX 2nd applied to non-apair")]))]
        [(isapair? e)
         (let ([v (eval-under-env (isapair-e e) env)])
            (if (apair? v)
              true
              false))]

        [(letrec? e)
         (let ([e1 (letrec-e1 e)]
               [s1 (letrec-s1 e)]
               [e2 (letrec-e2 e)]
               [s2 (letrec-s2 e)]
               [e3 (letrec-e3 e)]
               [s3 (letrec-s3 e)]
               [e4 (letrec-e4 e)]
               [s4 (letrec-s4 e)]
               [e5 (letrec-e5 e)])
         (if (and (string? (letrec-s1 e)) (string? (letrec-s2 e)) (string? (letrec-s3 e)) (string? (letrec-s4 e)))
             (eval-under-env e5 (cons (cons s1 e1)(cons (cons s2 e2)(cons (cons s3 e3) (cons (cons s4 e4) env)))))
             (error "NUMEX letrec applied to non-string")))]

        [(key? e)
         (let ([v (eval-under-env (key-e e) env)])
           (key key-s v))]
        [(record? e)
         (let ([v1 (eval-under-env (record-k e) env)]
               [v2 (eval-under-env (record-r e) env)])
           (cond [(key? v1) (cond [(or (munit? v2) (record? v2)) (record v1 v2)]
                                  [true (error "Second arguments is not (munit) nor record")])]
                 [true (error "First argument is not a key")]))]
         [(value? e)
          (let ([v2 (eval-under-env (value-r e) env)])
            (cond [(string? (value-s e)) (cond [(record? v2) (cond [(equal? (value-s e) (key-s (record-k v2))) (key-e (record-k v2))]
                                                          [(munit? (record-r v2)) (munit)]
                                                          [true (eval-under-env (value (value-s e) (record-r v2)) null)])]
                                      [true (error "Second value argument is not a record")])]
                  [true (error "First value argument is not a string")]))]
         ;[(ifneq? e)
          ;(cons (num 2) (num 3))]


        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3

(define (ifmunit e1 e2 e3)
  (cnd (ismunit e1) e2 e3))


(define (with* bs e2)
  (cond [(null? (car bs)) e2]
        [(null? (cdr bs)) (with (car (car bs)) (cdr (car bs)) e2)]
        [true (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2))]))


(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4 e3))
        
;; Problem 4

(define numex-filter (lam null "func" (lam "map" "lst" (let ([v (var "lst")])
                                                           (ifmunit v (munit) (ifnzero (apply (var "func") (1st v))
                                                                                       (apair (apply (var "func") (1st v)) (apply (var "map") (2nd v)))
                                                                                       (apply (var "map") (2nd v)) ))))))                                             

(define numex-all-gt
  (with "filter" numex-filter
       (lam null "number" (apply (var "filter") (lam null "x" (ifleq (var "x") (var "number") (num 0) (var "x")))))))


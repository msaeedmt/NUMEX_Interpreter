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


(define (ifmunit e1 e2 e3)
  (cnd (ismunit e1) e2 e3))


(define (with* bs e2)
  (cond [(null? (car bs)) e2]
        [(null? (cdr bs)) (with (car (car bs)) (cdr (car bs)) e2)]
        [true (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2))]))


(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4 e3))



;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function


;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (car (compute-free-vars-handler e))
)

(define (compute-free-vars-handler e)
  (cond [(var? e) (cons e (set (var-string e)))]
        [(num? e) (cons e (set))]
        [(bool? e) (cons e (set))]
        [(munit? e) (cons e (set))]
        [(key? e) (cons e (set))]
        [(closure? e) (cons e (set))]
        
        [(plus? e) 
         (let ([v1 (compute-free-vars-handler (plus-e1 e))]
               [v2 (compute-free-vars-handler (plus-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
        [(minus? e) 
         (let ([v1 (compute-free-vars-handler (minus-e1 e))]
               [v2 (compute-free-vars-handler (minus-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
        [(mult? e) 
         (let ([v1 (compute-free-vars-handler (mult-e1 e))]
               [v2 (compute-free-vars-handler (mult-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
        [(div? e) 
         (let ([v1 (compute-free-vars-handler (div-e1 e))]
               [v2 (compute-free-vars-handler (div-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
           
        [(andalso? e)
         (let ([v1 (compute-free-vars-handler (andalso-e1 e))]
               [v2 (compute-free-vars-handler (andalso-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
        [(orelse? e)
         (let ([v1 (compute-free-vars-handler (orelse-e1 e))]
               [v2 (compute-free-vars-handler (orelse-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
           
        [(neg? e)
         (let ([v (compute-free-vars-handler (neg-e e))])
           (cons e (cdr v)))]
                 
        [(cnd? e)
         (let ([v1 (compute-free-vars-handler (cnd-e1 e))]
               [v2 (compute-free-vars-handler (cnd-e2 e))]
               [v3 (compute-free-vars-handler (cnd-e3 e))])
           (cons e (set-union (cdr v1) (cdr v2) (cdr v3))))]
               
        [(iseq? e)
         (let ([v1 (compute-free-vars-handler (iseq-e1 e))]
               [v2 (compute-free-vars-handler (iseq-e2 e))])
           (cons e (set-union (cdr v2) (cdr v1))))]
        
        [(ifnzero? e)
         (let ([v1 (compute-free-vars-handler (ifnzero-e1 e))]
               [v2 (compute-free-vars-handler (ifnzero-e2 e))]
               [v3 (compute-free-vars-handler (ifnzero-e3 e))])
           (cons e (set-union (cdr v1) (cdr v2) (cdr v3))))]
           
        [(ifleq? e)
         (let ([v1 (compute-free-vars-handler (ifleq-e1 e))]
               [v2 (compute-free-vars-handler (ifleq-e2 e))]
               [v3 (compute-free-vars-handler (ifleq-e3 e))]
               [v4 (compute-free-vars-handler (ifleq-e4 e))])
           (cons e (set-union (cdr v1) (cdr v2) (cdr v3) (cdr v4))))]
        
        [(with? e)
         (let ([v1 (compute-free-vars-handler (with-e1 e))]
               [v2 (compute-free-vars-handler (with-e2 e))])
            (cons (with (with-s e) (car v1) (car v2)) (set-union (set-remove (cdr v2) (with-s e)) (cdr v1))))]
        
        [(ismunit? e) 
         (let ([v (compute-free-vars-handler (ismunit-e e))])
           (cons e (cdr v)))]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(lam? e)
         (let ([body (compute-free-vars-handler (lam-body e))])
           (let ([free-var-set (set-remove (set-remove (cdr body) (lam-formal e)) (lam-nameopt e))])
               (cons (fun-challenge (lam-nameopt e) (lam-formal e) (car body) free-var-set) free-var-set)))]

        [(apply? e)
         (let ([v (compute-free-vars-handler (apply-funexp e))]
               [arg (compute-free-vars-handler (apply-actual e))])
           (cons e (set-union (cdr v) (cdr arg))))]

        
        [(apair? e)
         (let ([v1 (compute-free-vars-handler (apair-e1 e))]
               [v2 (compute-free-vars-handler (apair-e2 e))])
           (cons e (set-union (cdr v1) (cdr v2))))]
        [(1st? e)
         (let ([v (compute-free-vars-handler (1st-e e))])
           (cons e (cdr v)))]
        [(2nd? e)
         (let ([v (compute-free-vars-handler (2nd-e e))])
           (cons e (cdr v)))]

        [(key? e)
          (let ([v (compute-free-vars-handler (key-e e))])
           (cons e (cdr v)))]
        [(record? e)
          (let ([v (compute-free-vars-handler (record-r e))])
           (cons e (cdr v)))]
         [(value? e)
          (let ([v (compute-free-vars-handler (value-s e))])
            (cons e (set-union (cdr v))))]

         [(letrec? e)
         (let ([v1 (compute-free-vars-handler (letrec-e1 e))]
               [v2 (compute-free-vars-handler (letrec-e2 e))]
               [v3 (compute-free-vars-handler (letrec-e3 e))])
           (cons e (set-union (cdr v1) (cdr v2) (cdr v3))))]

        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e)
         (let ([v (envlookup env (var-string e))])
           (cond [(or (num? v) (bool? v)) v]
                 [true (eval-under-env-c v env)]))]
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
         (let ([v1 (eval-under-env-c (plus-e1 e) env)]
               [v2 (eval-under-env-c (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        [(minus? e) 
         (let ([v1 (eval-under-env-c (minus-e1 e) env)]
               [v2 (eval-under-env-c (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]
        [(mult? e) 
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        [(div? e) 
         (let ([v1 (eval-under-env-c (div-e1 e) env)]
               [v2 (eval-under-env-c (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (quotient  (num-int v1) 
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]
        [(andalso? e)
         (let ([v1 (eval-under-env-c (andalso-e1 e) env)])
           (if (and (bool? v1) (equal? (bool-boolean v1) #f)) v1
               (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
                 (if (bool? v2) v2
                     (error "NUMEX andalso applied to non-bool")))))]

        [(orelse? e)
         (let ([v1 (eval-under-env-c (orelse-e1 e) env)])
           (if (and (bool? v1) (eq? (bool-boolean v1) #t)) v1
               (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
                 (if (bool? v2) v2
                     (error "NUMEX andalso applied to non-bool")))))]
        [(neg? e)
         (let ([v (eval-under-env-c (neg-e e) env)])
           (cond [(num? v) (num (- (num-int v)))]
                 [(bool? v) (cond ((equal? true (bool-boolean v)) (bool #f)) (true (bool #t)))]
                 [true (error "NUMEX negative applied to non-proper value")]))]
        [(cnd? e)
         (let ([v (eval-under-env-c (cnd-e1 e) env)])
           (if (bool? v) 
               (cond [(equal? (bool-boolean v) true) (eval-under-env-c (cnd-e2 e) env)]
                 [true (eval-under-env-c (cnd-e3 e) env)])
               (error "NUMEX cnd applied to a non-boolean condition")))]
        [(iseq? e)
         (let ([v1 (eval-under-env-c (iseq-e1 e) env)]
               [v2 (eval-under-env-c (iseq-e2 e) env)])
           (cond [(and (bool? v1) (bool? v2)) (cond [(equal? (bool-boolean v1) (bool-boolean v2)) (bool #t)]
                                                    [true (bool #f)])]
                 [(and (num? v1) (num? v2)) (cond [(equal? (num-int v1) (num-int v2)) (bool #t)]
                                                  [true (bool #f)])]
                 [true (bool #f)]))]
        [(ifnzero? e)
         (let ([v (eval-under-env-c (ifnzero-e1 e) env)])
           (if (num? v) 
               (cond [(equal? (num-int v) 0) (eval-under-env-c (ifnzero-e3 e) env)]
                     [true (eval-under-env-c (ifnzero-e2 e) env)])
               (error "NUMEX ifnzero applied to a non-number condition")))]
        [(ifleq? e)
         (let ([v1 (eval-under-env-c (ifleq-e1 e) env)]
               [v2 (eval-under-env-c (ifleq-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (cond [(> (num-int v1) (num-int v2)) (eval-under-env-c (ifleq-e4 e) env)]
                     [true (eval-under-env-c (ifleq-e3 e) env)])
               (error "NUMEX ifleq applied to non-number")))]
        [(with? e)
         (let ([v1 (eval-under-env-c (with-e1 e) env)])
           (eval-under-env-c (with-e2 e) (cons (cons (with-s e) v1) env)))]
        
        [(ismunit? e) 
         (let ([v1 (eval-under-env-c (ismunit-e e) env)])
           (if (munit? v1) (bool #t)
               (bool #f)))]

        [(lam? e)
         (cond [(null? (lam-nameopt e)) (closure env e)]
               [true (closure (cons (cons (lam-nameopt e) e) env) e)])]

        [(fun-challenge? e)
         (let ([freevars (fun-challenge-freevars e)])
           (cond [(null? (fun-challenge-nameopt e)) (closure (subscription env freevars) e)]
                 [true (closure (subscription (cons (cons (fun-challenge-nameopt e) e) env) freevars) e)]))]

        [(apply? e)
         (let ([v (eval-under-env-c (apply-funexp e) env)]
               [arg (eval-under-env-c (apply-actual e) env)])
           (cond [(closure? v) (eval-under-env-c (lam-body (closure-f v)) (cons (cons (lam-formal (closure-f v)) arg) (closure-env v)))]
                 [true (error "first argument should return a closure")]))]

        
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(1st? e)
         (let ([v (eval-under-env-c (1st-e e) env)])
           (cond [(apair? v) (apair-e1 v)]
                 [true (error "NUMEX 1st applied to non-apair")]))]
        [(2nd? e)
         (let ([v (eval-under-env-c (2nd-e e) env)])
           (cond [(apair? v) (apair-e2 v)]
                 [true (error "NUMEX 2nd applied to non-apair")]))]
        [(isapair? e)
         (let ([v (eval-under-env-c (isapair-e e) env)])
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
             (eval-under-env-c e5 (cons (cons s1 e1)(cons (cons s2 e2)(cons (cons s3 e3) (cons (cons s4 e4) env)))))
             (error "NUMEX letrec applied to non-string")))]

        [(key? e)
         (let ([v (eval-under-env-c (key-e e) env)])
           (key key-s v))]
        [(record? e)
         (let ([v1 (eval-under-env-c (record-k e) env)]
               [v2 (eval-under-env-c (record-r e) env)])
           (cond [(key? v1) (cond [(or (munit? v2) (record? v2)) (record v1 v2)]
                                  [true (error "Second arguments is not (munit) nor record")])]
                 [true (error "First argument is not a key")]))]
         [(value? e)
          (let ([v2 (eval-under-env-c (value-r e) env)])
            (cond [(string? (value-s e)) (cond [(record? v2) (cond [(equal? (value-s e) (key-s (record-k v2))) (key-e (record-k v2))]
                                                          [(munit? (record-r v2)) (munit)]
                                                          [true (eval-under-env-c (value (value-s e) (record-r v2)) null)])]
                                      [true (error "Second value argument is not a record")])]
                  [true (error "First value argument is not a string")]))]
         ;[(ifneq? e)
          ;(cons (num 2) (num 3))]


        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

(define (subscription env set)
  (cond [(equal? env null) null]
        [true (cond [(set-member? set (car (car env))) (cons (car env) (subscription (cdr env) set))]
                    [true (subscription (cdr env) set)])]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct bool (b)      #:transparent)  ;; a boolean value, e.g., (bool #t)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg  (e1)     #:transparent)  ;; negate the expression
(struct islthan (e1 e2) #:transparent) ;; is less than
(struct ifzero (e1 e2 e3) #:transparent) ;; tests e1
(struct ifgthan (e1 e2 e3 e4) #:transparent) ;; tests if e1 is greater than e2
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (s e1 e2)  #:transparent) ;; a local bounder which the value of e1 is bound to s in the expression e2
(struct apair (e1 e2)   #:transparent) ;; pair constructor
(struct first (e1)      #:transparent) ;; the first element of the pair e1
(struct second (e2)     #:transparent) ;; the second element of the pair e2


(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 

;; Problem 1

(define (racketlist->numexlist xs) (cond [(null? xs)(munit)]
                                         [(true)(apair(car xs)(racketlist->numexlist (cdr xs)))]))
(define (numexlist->racketlist xs) (cond [(munit? xs)(null)]
                                         [(true)(cons (car xs)(numexlist->racketlist(cdr xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
  	[(eq? str (car(car env))) (cdr(car env))]
        [true (envlookup (cdr env) str)]))

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int v1)
                    (int v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "NUMEX addition applied to non-number")))]

        
        [(int? e)
         (if (integer? e) e (error "not integer"))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1) 
                       (int-num v2)))
               (error "NUMEX multipation applied to non-number")))]

        [(neg? e)
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (if (int? v1)
               (int (- (int-num v1)))
               (error "NUMEX negation applied to non-number")))]

        [(islthan? e)
         (let ([v1 (eval-under-env (islthan-e1 e) env)]
               [v2 (eval-under-env (islthan-e2 e) env)])
           (if (and (int? v1)(int? v2))
               (if (< v1 v2)(int 1)(int 0))
               (error "wrong format!")))]

        [(ifzero? e)
         (let ([v1 (eval-under-env (ifzero-e1 e) env)])
           (if (int? v1)
               (if (zero? v1)
                   (eval-under-env (ifzero-e2 e) env)
                   (eval-under-env (ifzero-e3 e) env))
               (error "wrong format")))]

        [(ifgthan? e)
         (let ([v1 (eval-under-env (ifgthan-e1 e) env)]
               [v2 (eval-under-env (ifgthan-e2 e) env)])
           (if (and (int? v1)(int? v2))
               (if (> v1 v2)
                   (eval-under-env (ifgthan-e3 e) env)
                   (eval-under-env (ifgthan-e4 e) env))
               (error "wrong format")))]

        [(apair? e)
         (let([v1 (eval-under-env (apair-e1 e) env)]
              [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(first? e)
         (let([v1 (eval-under-env (first-e1 e) env)])
           (if (apair? v1)
               (eval-under-env (apair-e1 e) env)
               (error "not a pair")))]
         
        [(second? e)
         (let([v1 (eval-under-env (second-e2 e) env)])
           (if (apair? v1)
               (eval-under-env (apair-e2 e) env)
               (error "not a pair")))]
              
         
        [(munit? e)
           (munit)]

        [(ismunit? e)
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (if (munit? v1)(int 1)(int 0)))]

        [(closure? e) e]

        [(fun? e)
             (closure env e)]

        [(call? e)]
        
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define numex-filter "CHANGE")

;;(define numex-all-gt
;;  (with "filter" numex-filter
;;        "CHANGE (notice filter is now in NUMEX scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

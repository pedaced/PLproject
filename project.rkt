;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct bool (b)      #:transparent)  ;; a boolean value, e.g., (bool #t)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)  ;; subtracts two expressions
(struct div  (e1 e2)  #:transparent)  ;; divides two expressions
(struct mult (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg  (e1)     #:transparent)  ;; negate the expression
(struct iseq  (e1 e2) #:transparent)  ;;checks whether e1 and e2 are equal
(struct andalso (e1 e2) #:transparent) ;; and two booleans
(struct orelse (e1 e2) #:transparent) ;; or two booleans
(struct cnd (e1 e2 e3) #:transparent) ;; tests e1
(struct ifnzero (e1 e2 e3) #:transparent) ;; tests e1
(struct ifleq (e1 e2 e3 e4) #:transparent) ;; tests if e1 is greater than e2
(struct with (s e1 e2)  #:transparent) ;; a local bounder which the value of e1 is bound to s in the expression e2
(struct apair (e1 e2)   #:transparent) ;; pair constructor
(struct 1st (e1)      #:transparent) ;; the first element of the pair e1
(struct 2nd (e2)     #:transparent) ;; the second element of the pair e2


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
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num v1)
                    (num v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        
        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num v1)
                    (num v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num v1)
                    (num v2))
               (num (/ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX division applied to non-number")))]
        
        
        [(num? e)
         (if (integer? e) e (error "not integer"))]

        
        [(bool? e)
         (if (boolean? e) e (error "not boolean"))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multipation applied to non-number")))]

        [(iseq? e) 
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (if (and (num? v1)(num? v2))
               (if (eq? (num-int v1)(num-int v2))#t
               (if (eq? (num-int v1)(num-int v2))#t #f)) 
               (error "wrong format")))]

        

        [(neg? e)
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (if (num? v1)
               (num (- (num-int v1)))
               (error "NUMEX negation applied to non-number")))]
        
        
        [(andalso? e) 
         (let ([v1 (eval-under-env (andalso-e1 e) env)]
               [v2 (eval-under-env (andalso-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (and (bool-b v1) 
                       (bool-b v2)))
               (error "NUMEX conjunction applied to non-boolean")))]
        
        [(orelse? e) 
         (let ([v1 (eval-under-env (orelse-e1 e) env)]
               [v2 (eval-under-env (orelse-e2 e) env)])
           (if (and (bool? v1)
                    (bool? v2))
               (bool (or (bool-b v1) 
                       (bool-b v2)))
               (error "NUMEX injunction applied to non-boolean")))]

        
        [(cnd? e)
         (let ([v1 (eval-under-env (cnd-e1 e) env)])
           (if (boolean? v1)
               (if (equal? v1 #t)
                   (eval-under-env (cnd-e2 e) env)
                   (eval-under-env (cnd-e3 e) env))
               (error "wrong format")))]

        [(ifnzero? e)
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if (num? v1)
               (if (zero? v1)
                   (eval-under-env (ifnzero-e2 e) env)
                   (eval-under-env (ifnzero-e3 e) env))
               (error "wrong format")))]

        [(ifleq? e)
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if (and (num? v1)(num? v2))
               (if (> v1 v2)
                   (eval-under-env (ifleq-e3 e) env)
                   (eval-under-env (ifleq-e4 e) env))
               (error "wrong format")))]

        [(apair? e)
         (let([v1 (eval-under-env (apair-e1 e) env)]
              [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(1st e)
         (let([v1 (eval-under-env (1st-e1 e) env)])
           (if (apair? v1)
               (eval-under-env (apair-e1 e) env)
               (error "not a pair")))]
         
        [(2nd? e)
         (let([v1 (eval-under-env (2nd-e2 e) env)])
           (if (apair? v1)
               (eval-under-env (apair-e2 e) env)
               (error "not a pair")))]
              
          [(with? e)
         (define sName (with-s e))
         (let ([v1 (eval-under-env (with-e1 e) env)])
           (eval-under-env (with-e2 e) (cons (cons sName v1) env)))]
          
        [(munit? e)
           (munit)]

        [(ismunit? e)
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (if (munit? v1)(bool #t)(bool #f)))]

        [(closure? e) e]

        [(apply? e)
             (closure env e)]
        
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
  (if (munit? e1) e2 e3))


(define (with* bs e2)(cond [(null? bs) (with "finalExpResult" (munit) e2)] [true (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2))]))

(define (ifneq e1 e2 e3 e4)
   (let ([v1 e1]
         [v2 e2])
  (ifleq v1 v2 e4 (ifleq v2 v1 e4 e3))))

;; Problem 4

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

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

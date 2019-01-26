#lang racket

(require "../common.rkt")
(require "../programs.rkt")
(require "enode.rkt")
(require "egraph.rkt")

(provide match-e substitute-e)

;;################################################################################;;
;;# The matcher module that allows us to match enode structure against patterns,
;;# giving you the bindings between enodes and variables in the pattern, and
;;# substitute those bindings into patterns, creating new enodes.
;;#
;;# This is copy-pasted from the old egraphs file, with minor modifications,
;;# so it might not always work correctly.
;;#
;;################################################################################;;

(define (merge . bindings)
  ;; (list bindings) -> binding
(begin 
  (foldl merge2 '() bindings)))

(define (merge2 binding1 binding2)
  ;; binding binding -> binding

(define (assocs k l) (filter (lambda (kv) (equal? k (car kv))) l))

(begin (print 'merg);(print binding1) (print binding2)

  (if (and binding1 binding2)
      (let loop ([acc binding1] [rest binding2])
	(begin (print acc ) (print rest)(if (null? rest)
	    acc
	    (let* ([curr (car rest)]
		   [lookup (assocs (car curr) acc)])
	      (begin (print 'look)(print lookup) (if (not (empty? lookup)) (begin 
		   (if (printret (all? (printret (map (lambda (l) (newmatch (cdr l) (cdr curr))) lookup))))
                     (if (shouldadd lookup  (cdr curr))
(loop (cons curr acc) (cdr rest))
		      (begin (loop acc (cdr rest))))
		      #f))
		  (loop (cons curr acc) (cdr rest))))))))
      #f)))

(define (shouldadd l c) (or (bothnot c) (empty? (filter (lambda (e) (not (equal? (cadr e) 'not))) l))))

(define (all? ls) (foldl (lambda (x y) (and x y)) #t ls))

(define (bothnot b)
  (match b[ (cons 'not y) #t]
[ other #f]))

(define (consnot c ac) 
  (begin (match (cdr c) [(cons 'not x) ac][x (cons c ac)])))

(define (denot x)
  (filter (lambda (b) (match (cdr b) [(cons 'not w) #f] [other #t])) x))

(define (newmatch a b)
(begin (print 'matvch) (print a)(print b)
  (printret (match (cons a b) 
   [(cons (cons 'not ab) (cons 'not bb)) #t]
   [(cons (cons 'not ab) bb) (not (equal? ab bb))]
   [(cons ab (cons 'not bb)) (not (equal? ab bb))]
   [(cons ab bb) (equal? ab bb)]))))

(define (match-e pat e) 
  (map denot (match-e0 pat e)))

(define (match-e0 pat e)
  (cond
   [(constant? pat)
    (call/ec
     (λ (k)
       (for ([var (in-set (enode-vars e))])
	 (when (and (constant? var) (equal? pat var))
	   (k '(()))))
       '()))]
   [(variable? pat)
    `(((,pat . ,e)))]
   [(equal? (car pat) 'isnt)
    `(((,(cadr pat) .  ,e) (,(caddr pat) . (not . ,e))))]
   [(list? pat)
    (apply append
	   (for/list ([var (in-set (enode-vars e))])
	     (if (and (list? var) (eq? (car var) (car pat))
		      (= (length var) (length pat)))
		 (filter identity
			 (map (curry apply merge)
			      (apply cartesian-product
				     (for/list ([subpat (in-list (cdr pat))] [sube (in-list (cdr var))])
				       (match-e0 subpat sube)))))
		 '())))]
   [else
    (error "WTF" pat)]))

(define (printret x) (begin (print x) x))

(define (substitute-e eg pat bindings)
(begin (print 'subst)(print pat) (print bindings) (print eg)
  (cond
   [(constant? pat)
    (mk-enode! eg pat)]
   [(variable? pat)
    (let ([binden (cdr (assoc pat bindings))])
      binden)]
   [(list? pat)
    (mk-enode! eg (cons (car pat)
			(for/list ([subpat (cdr pat)])
			  (substitute-e eg subpat bindings))))])))

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

(define (in-schema i u)
  (let ([es (set->list (enode-vars u))])
       (any? (lambda (e) (in-exp i e)) es)))

(define (in-exp i e)
 (match e
  [(? symbol?) (equal? i e)]
  [(list 'agg ind bod) (if (equal? i (enode-expr ind))
                           #f
                           (in-schema i bod))]
  [(list op ens ...) (any? (lambda (e) (in-schema i e)) ens)]
  [x #f]))

(define (merge . bindings)
  ;; (list bindings) -> binding
  (foldl merge2 '() bindings))

(define (merge2 binding1 binding2)
  ;; binding binding -> binding
  ;; look up all values that have key k
  (define (assocs k l) (filter (lambda (kv) (equal? k (car kv))) l))
  (if (and binding1 binding2)
      (let loop ([acc binding1] [rest binding2])
        (if (null? rest)
            acc
            (let* ([curr (car rest)]
                   [lookup (assocs (car curr) acc)])
              (if (not (empty? lookup))
                  ;; if all bindings are consistent
                  (if (all? (map (lambda (l) (newmatch (cdr l) (cdr curr))) lookup))
                      (if (shouldadd lookup  (cdr curr))
                          (loop (cons curr acc) (cdr rest))
                          (loop acc (cdr rest)))
                      #f)
                  (loop (cons curr acc) (cdr rest))))))
      #f))

;; add binding c to l if l doesn't bind it
(define (shouldadd l c)
  (or (isnot c)
      (empty? (filter (lambda (e) (or (not (pair? (cdr e)))
                                      (and (not (equal? (cadr e) 'not))
                                           (not (equal? (cadr e) 'notin))
                                           (not (equal? (cadr e) 'isin)))))
                      l))))

(define (all? ls) (foldl (lambda (x y) (and x y)) #t ls))

(define (any? f ls) (foldl (lambda (x y) (or (f x) y)) #f ls))

(define (isnot b)
  (match b
    [(cons 'not y) #t]
    [(cons 'notin y) #t]
    [(cons 'isin y) #t]
    [other #f]))

(define (denot x)
  (filter (lambda (b) (match (cdr b) [(cons 'isin w) #f] [(cons 'notin w) #f] [(cons 'not w) #f] [other #t])) x))

;; check if two matches are consistent
(define (newmatch a b)
  (match (cons a b)
    [(cons (cons 'isin ab) (cons 'isin bb)) #t]
    [(cons (cons 'isin ab) bb) (in-schema (enode-expr bb) ab)]
    [(cons ab (cons 'isin bb)) (in-schema (enode-expr ab) bb)]
    [(cons (cons 'isin ab) (cons 'notin ab)) #f]
    [(cons (cons 'isin ab) (cons 'notin bb)) #t]
    [(cons (cons 'notin ab) (cons 'notin bb)) #t]
    [(cons (cons 'notin ab) bb) (not (in-schema (enode-expr bb) ab))]
    [(cons ab (cons 'notin bb)) (not (in-schema (enode-expr ab) bb))]
    [(cons ab bb) (equal? ab bb)]))

(define (match-e pat e)
  ;; remove "not" bindings
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
    [(equal? (car pat) 'hasnt)
     `(((,(cadr pat) .  ,e) (,(caddr pat) . (notin . ,e))))]
    [(equal? (car pat) 'has)
     `(((,(cadr pat) .  ,e) (,(caddr pat) . (isin . ,e))))]
    #;[(equal? (car pat) 'r!)
     (call/ec
      (λ (k)
        (for ([var (in-set (enode-vars e))])
          (when (constant? var) 
            (k `(((,(cadr pat) . ,e))))))
        '()))]
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

;(define (rename? p) (or (equal? p 'rename) (equal? (car p) 'rn1) (equal? (car p) 'rn2)))
(define (rename? p) (equal? (car p) 'rename))
(define (rn-rule p) (if (equal? p 'rn1) 'raggrename 'raggrename2))
(define (foundit? p) (equal? (car p) 'foundit))

(define (substitute-e eg pat bindings)
  (cond
    [(constant? pat) 
     (mk-enode! eg pat)]
    [(equal? pat 'freshname!)
     (mk-enode! eg (gensym))]
    [(equal? pat 'fold*)
     (let* ([xn (cdr (assoc 'xconstant bindings))]
            [yn (cdr (assoc 'yconstant bindings))]
            [x (enode-expr xn)]
            [y (enode-expr yn)])
       (if (and (constant? x) (constant? y))
           (mk-enode! eg (* x y))
           (mk-enode! eg `(r* ,xn ,yn))))]
    [(equal? pat 'fold+)
     (let* ([xn (cdr (assoc 'xconstant bindings))]
            [yn (cdr (assoc 'yconstant bindings))]
            [x (enode-expr xn)]
            [y (enode-expr yn)])
       (if (and (constant? x) (constant? y))
           (mk-enode! eg (+ x y))
           (mk-enode! eg `(r+ ,xn ,yn))))]
    [(variable? pat)
     (let ([binden (cdr (assoc pat bindings))]) 
       binden)]
    [(foundit? pat) (println (cadr pat))]
    [(rename? pat)
     (let* (;[binden (cdr (assoc (cadr pat) bindings))]
            [ii (begin (println 'gen) (gensym))]
            [res (mk-enode! eg (list 'agg (mk-enode! eg ii) (mk-enode! eg (list 'r*
                                               (substitute-e eg 'u bindings)
                                               (mk-enode! eg `(-/- ,(substitute-e eg 'v bindings) 
                                                                ,(substitute-e eg 'a bindings) ,(mk-enode! eg ii)))))))])
       res)]
    [(list? pat)
     (mk-enode! eg (cons (car pat)
                         (for/list ([subpat (cdr pat)])
                           (substitute-e eg subpat bindings))))]))

#;(define (rrename-enode eg eno i ii)
  (let loop! [(en (pack-leader eno))]
    (let* ([ncs (map loop! (enode-children en))]
           [newen (mk-enode! eg (cons op (map (lambda (e) (loop! e)) ens)))])
      )
    (match (enode-expr en)
      [(? symbol?) (if (equal? (enode-expr en) i) (mk-enode! eg ii) en)]
      [(list op ens ...) (mk-enode! eg (cons op (map (lambda (e) (loop! e)) ens)))]
      [_ en])))

(define (rrename-enode eg eno i ii)
  (foldr (lambda (en en0)
           (merge-egraph-nodes! eg (mk-enode! eg (rrename-expr eg (enode-expr en) i ii)) en0))
         (mk-enode! eg (rrename-expr eg (enode-expr (pack-leader eno)) i ii))
         (enode-children (pack-leader eno))))

(define (rrename-expr eg exp i ii)
  (match exp
    [(? symbol?) (if (equal? exp i) ii exp)]
    [(list op ens ...) (cons op (map (lambda (e) (rrename-enode eg e i ii)) ens))]
    [e e]))

#;(define (for-pack! f en)
  (let loop! ([en (pack-leader en)])
    (let ([children* (map loop! (enode-children en))])
      (set-enode-children! en children*)
      (f en)
      (set-enode-cvars! en (apply set-union (set (enode-expr en))
                                  (map enode-cvars (enode-children en))))
      en)))

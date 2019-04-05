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
      (empty? (filter (lambda (e) (or
                                   (not (pair? (cdr e)))
                                   (not (equal? (cadr e) 'not))))
                      l))))

(define (all? ls) (foldl (lambda (x y) (and x y)) #t ls))

(define (isnot b)
  (match b
    [(cons 'not y) #t]
    [other #f]))

(define (denot x)
  (filter (lambda (b) (match (cdr b) [(cons 'not w) #f] [other #t])) x))

;; check if two matches are consistent
(define (newmatch a b)
  (match (cons a b)
   [(cons (cons 'not ab) (cons 'not bb)) #t]
   [(cons (cons 'not ab) bb) (not (equal? ab bb))]
   [(cons ab (cons 'not bb)) (not (equal? ab bb))]
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

(define (rename? p) (equal? (car p) 'rn))
(define (foundit? p) (equal? (car p) 'foundit))

(define (substitute-e eg pat bindings)
  (println "in sub")
  (cond
    [(constant? pat) (println "in const")
     (mk-enode! eg pat)]
    [(variable? pat) (println "in var")
     (let ([binden (cdr (assoc pat bindings))]) (println "got it")
       binden)]
    ;[(foundit? pat) (print "DAWWG")]
    [(rename? pat)
     (println "rename")
     (let* ([binden (cdr (assoc (cadr pat) bindings))]
            [irn (enode-expr (cdr (assoc (caddr pat) bindings)))] ; index to rename
            [ii (gensym irn)]) ; fresh index name
       (mk-enode! eg (list 'agg (mk-enode! eg ii)
                           (mk-enode! eg (list 'r*
                                               (substitute-e eg '(b+ a (: i l)) bindings)
                                               (rrename-enode eg binden irn ii))))))]
    [(list? pat)
     (mk-enode! eg (cons (car pat)
                         (for/list ([subpat (cdr pat)])
                           (substitute-e eg subpat bindings))))]))

(define (rrename-enode eg eno i ii)
  (let loop! [(en (pack-leader eno))]
    (match (enode-expr en)
      [(? symbol?) (if (equal? (enode-expr en) i) (mk-enode! eg ii) en)]
      [(list op ens ...) (mk-enode! eg (cons op (map (lambda (e) (loop! e)) ens)))]
      [_ en])))

#;(define (for-pack! f en)
  (let loop! ([en (pack-leader en)])
    (let ([children* (map loop! (enode-children en))])
      (set-enode-children! en children*)
      (f en)
      (set-enode-cvars! en (apply set-union (set (enode-expr en))
                                  (map enode-cvars (enode-children en))))
      en)))

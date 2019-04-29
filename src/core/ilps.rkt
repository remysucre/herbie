#lang rosette

(require "enode.rkt")
(require "egraph.rkt")
(require "simplify.rkt")
(require rosette/solver/mip/cplex)

(define (gen-variables eg)
  (define node->var (make-hash))
  (define pack->var (make-hash))
  (define (traverse-pack parent)
    (unless (or (not (list? parent))
                (null? parent)
                (let ([op (car (enode-expr parent))])
                  (or (eq? op ':) (eq? op 'b+))))
      (define-symbolic* x integer?)
      (define-symbolic* b integer?)
      (hash-set! node->var parent (cons x b)))
    (for ([child (enode-children parent)])
      (traverse-pack child)))
  (for ([leader (egraph-leaders eg)])
    (define-symbolic* p integer?)
    (hash-set! pack->var leader p)
    (traverse-pack leader))
  (values node->var pack->var))

(define (make-assertions node-map packs-map)
  (define (get-cost subexpr)
    (if (enode? subexpr)
        (hash-ref packs-map (pack-leader subexpr))
        0))
  (for ([(curr-enode vars) node-map])
    (let* ([b (cdr vars)]
           [x (car vars)]
           [expr (enode-expr curr-enode)])
      (assert (&& (<= 0 b) (>= 1 b)))
      (match expr
        [`(: ,a ,b)   (error "Binding in map")]
        [`(b+ ,m ,ds) (error "Binding in map")]
        
        [(list-rest 'r+ children)
         (assert (>= x
                  (+ (length (cdr children))
                     (apply + (map get-cost children)))))]
        [(list-rest 'r* children)
         (assert (>= x
                     (+ (length (cdr children))
                        (apply + (map get-cost children)))))]
        [`(agg ,dimensions ,tensor)
         (assert (>= x (+ 1 (get-cost tensor))))]))
    (letrec ([get-pack (λ (x) (cons x (append-map get-pack (enode-children x))))])
      (for ([(pack-leader p) packs-map])
        (assert (= p (apply +
                            (map (λ (pair) (* (car pair) (cdr pair)))
                                 (map (λ (node) (hash-ref node-map node))
                                      (get-pack pack-leader))))))))))

(define (egraph->plan eg pack-leader-to-minimze)
  (define old-solver (current-solver))

  (define ilp-solver (cplex #:path "/opt/ibm/ILOG/CPLEX_Studio_Community128/cplex/bin/x86-64_linux/cplex"))
  (current-solver ilp-solver)
  (define-values (node-map pack-map) (gen-variables eg))
  (make-assertions node-map pack-map)
  (solver-minimize ilp-solver (list (hash-ref pack-map pack-leader-to-minimze)))
  ;(define soln (solver-check ilp-solver))
  
  ;(current-solver old-solver)
  ilp-solver)
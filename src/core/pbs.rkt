#lang rosette

(require "enode.rkt")
(require "egraph.rkt")
(require "simplify.rkt")
(require rosette/solver/mip/cplex)

(define (gen-variables eg)
  (define node->var (make-hash))
  (define pack->var (make-hash))
  (define (genevar en)
    (let ([e (enode-expr en)])
      (unless (or (not (list? e))(eq? (car e) ':) (eq? (car e) 'b+))
        (define-symbolic* x integer?)
        (hash-set! node->var e x))))
  
  #;(define (traverse-pack parent)
    (let ([pe (enode-expr parent)])
    (unless (or (not (list? pe))
                (null? pe)
                (let ([op (car (enode-expr parent))])
                  (or (eq? op ':) (eq? op 'b+))))
      (define-symbolic* x integer?)
      (hash-set! node->var parent x)))
    (for ([child (enode-children parent)])
      (traverse-pack child)))
  (for ([leader (egraph-leaders eg)])
    (define-symbolic* p integer?)
    (hash-set! pack->var leader p)
    (for-pack! genevar leader))
  (values node->var pack->var))

(define (debugpr e) (println e) e)

(define (make-assertions node-map packs-map)
  (for ([(expr bn) node-map])
      (assert (|| (= 0 bn) (= bn 1)))
      (match expr
        [`(: ,a ,b)   (error "Binding in map")]
        [`(b+ ,m ,ds) (error "Binding in map")]
        [(list-rest 'r+ children)
         (assert (=> (= bn 1) (apply && (map (λ (c) (= (hash-ref packs-map c) 1)) children))))]
        [(list-rest 'r* children)
         (assert (=> (= bn 1) (apply && (map (λ (c) (= (hash-ref packs-map c) 1)) children))))]
        [`(agg ,dimensions ,tensor)
         (assert (=> (= bn 1) (= (hash-ref packs-map tensor) 1)))]))
  (for ([(pack bq) packs-map])
    (assert (|| (= 0 bq) (= bq 1)))
    (let ([cs (map (lambda (c) (= (hash-ref node-map (enode-expr c) 1) 1))
                   (pack-members pack))])
      (assert (=> (= bq 1) (apply || cs))))))
    

(define (egraph->plan eg pack-leader-to-minimze)
  ;(define old-solver (current-solver))

  ;(define ilp-solver (cplex #:path "/Applications/CPLEX_Studio129/cplex/bin/x86-64_osx/cplex"))
  ;(current-solver ilp-solver)
  (define-values (node-map pack-map) (gen-variables eg))
  (make-assertions node-map pack-map)
  ;(println (hash-values node-map))
  ;(solver-minimize ilp-solver (list (apply + (hash-values node-map))))
  ;(asserts)
  (define soln (optimize #:minimize (list (apply + (hash-values node-map))) #:guarantee (assert #t)))
  ;(current-solver old-solver)
  #;(begin (define g (mk-egraph '(r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                      (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))
  (egraph->plan g (pack-leader (egraph-top g))))
  (model soln))

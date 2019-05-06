#lang rosette

(require "enode.rkt")
(require "egraph.rkt")
(require "simplify.rkt")
(require "../syntax/rules.rkt")
(require rosette/solver/mip/cplex)

(define (gen-variables eg)
  (define node->var (make-hash))
  (define pack->var (make-hash))
  ; generate expression variables
  (define (genevar en)
      (define-symbolic* x boolean?)
      (hash-set! node->var en x))
  ; generate pack variables
  (define (genpvar pack)
    (define-symbolic* p boolean?)
    (hash-set! pack->var pack p)
    (for-pack! genevar pack))
  (map-enodes genpvar eg)
  (values node->var pack->var))

(define (debugpr e) (println e) e)

(define (make-assertions eg node-map packs-map)
  (assert (hash-ref packs-map (pack-leader (egraph-top eg))))
  (for ([(en bn) node-map])
      (match (enode-expr en)
        [(? symbol?) (void)]
        [(? real?) (void)]
        [(list op ens ...) (println op)
         (assert (=> bn (apply && (map (λ (c) (hash-ref packs-map (pack-leader c))) ens))))]))
  (for ([(pack bq) packs-map])
    (let ([cs (map (curry hash-ref node-map) (pack-members pack))])
      (assert (=> bq (apply || cs))))))

(define (toint b) (if b 1 0))


(define (egraph->plan eg)
  (define-values (node-map pack-map) (gen-variables eg))
  (make-assertions eg node-map pack-map)
  (define soln (optimize #:minimize (list (apply + (map toint (hash-values node-map))))
                         #:guarantee (assert #t)))

  (define (extractp pack)
    (car (for/list ([p (pack-members pack)]
                    #:when (evaluate (hash-ref node-map p) soln))
           (extracte p))))

  (define (extracte e)
    (match (enode-expr e)
      [(? symbol?) (enode-expr e)]
      [(? real?) (enode-expr e)]
      [(list op ens ...) (cons op (map extractp ens))]))

  ;(map (lambda (k) (evaluate k soln)) (hash-values pack-map) )
  (extractp (pack-leader (egraph-top eg))))

#;(begin (define g (mk-egraph '(r* (b+ u (: a b)) (b+ u (: a b)))))
         (define ans(egraph->plan g (pack-leader (egraph-top g)))))

#;(begin (define g (mk-egraph '(r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                   (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))
         (define ans (egraph->plan g (pack-leader (egraph-top g)))))

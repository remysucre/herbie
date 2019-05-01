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
    (let ([e (enode-expr en)])
      (unless (or (not (list? e)) (eq? (car e) ':) (eq? (car e) 'b+))
        (define-symbolic* x boolean?)
        (hash-set! node->var e x))))
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
  (for ([(expr bn) node-map])
      (match expr
        [`(: ,a ,b)   (error "Binding in map")]
        [`(b+ ,m ,ds) (error "Binding in map")]
        [(list-rest 'r+ children)
         (assert (=> bn (apply && (map (λ (c) (hash-ref packs-map c)) children))))]
        [(list-rest 'r* children)
         (assert (=> bn (apply && (map (λ (c) (hash-ref packs-map c)) children))))]
        [`(agg ,dimensions ,tensor)
         (assert (=> bn (hash-ref packs-map tensor)))]))
  (for ([(pack bq) packs-map])
    (let ([cs (map (lambda (c) (hash-ref node-map (enode-expr c) #t))
                   (pack-members pack))])
      (assert (=> bq (apply || cs))))))

(define (toint b) (if b 1 0))

(define (extractp sol pack)
  (car (map (curry extracte sol) (pack-members pack))))

(define (extracte sol e)
      (match (enode-expr e)
        [`(: ,a ,b) `(: ,(enode-expr a) ,(enode-expr b))]
        [`(b+ ,m ,ds) `(b+ ,(enode-expr m) ,(extractp sol ds))]
        [(list-rest 'r+ children)
         (if (evaluate sol (enode-expr e)) (cons 'r+ (map (curry extractp sol) children)) '())]
        [(list-rest 'r* children)
         (if (evaluate sol (enode-expr e)) (cons 'r* (map (curry extractp sol) children)) '())]
        [`(agg ,dimensions ,tensor)
         (if (evaluate sol (enode-expr e)) `(agg ,(enode-expr dimensions) ,(extractp sol tensor))'())]))

(define (egraph->plan eg)
  (define-values (node-map pack-map) (gen-variables eg))
  (make-assertions eg node-map pack-map)
  (define soln (optimize #:minimize (list (apply + (map toint (hash-values node-map))))
                         #:guarantee (assert #t)))
  (model soln)
  (extractp soln (egraph-top eg)))

#;(begin (define g (mk-egraph '(r* (b+ u (: a b)) (b+ u (: a b)))))
         (define ans(egraph->plan g (pack-leader (egraph-top g)))))

#;(begin (define g (mk-egraph '(r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                   (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))
         (define ans (egraph->plan g (pack-leader (egraph-top g)))))

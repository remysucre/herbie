#lang racket

(define ga '(r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                      (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))

(define g0 '(agg a (agg c (r* (r+ (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))
                              (r+ (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))))

(define g1 '(agg a (agg c (r+ (r+ (r* (b+ x (: a c)) (b+ x (: a c)))
                                  (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))
                              (r+ (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))
                                  (r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                      (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))))))

(define g2 '(r+ (r+ (agg a (agg c (r* (b+ x (: a c)) (b+ x (: a c)))))
                    (agg a (agg c (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))))
                (r+ (agg a (agg c (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))
                    (agg a (agg c (r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                      (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))))))

(define g3 '(r+ (r+ (agg a (agg c (r* (b+ x (: a c)) (b+ x (: a c)))))
                    (agg a (agg c (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))))
                (r+ (agg a (agg c (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))
                    (agg a (agg c (agg b (agg d (r* (r* (b+ u (: a b)) (b+ v (: c b))) (r* (b+ u (: a d)) (b+ v (: c d)))))))))))

(define g4 '(r+ (r+ (agg a (agg c (r* (b+ x (: a c)) (b+ x (: a c)))))
                    (agg a (agg c (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))))
                (r+ (agg a (agg c (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))
                    (agg b (agg d (r* (agg a (r* (b+ u (: a b)) (b+ u (: a d))))
                                      (agg c (r* (b+ v (: c d)) (b+ v (: c b))))))))))

(define g5 '(r+ (agg a (agg c (r+ (r* (b+ x (: a c)) (b+ x (: a c)))
                                  (r+ (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))
                                      (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))))
                (agg b (agg d (r* (agg a (r* (b+ u (: a b)) (b+ u (: a d))))
                                  (agg c (r* (b+ v (: c d)) (b+ v (: c b)))))))))
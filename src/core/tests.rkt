#lang racket

; TR(ABC) = TR(CA trans(B))
(define e0 '(agg i (agg k (r* (agg j (r* (b+ A (: i j)) (b+ B (: j k)))) (b+ C (: k i))))))

(define e0e '(agg j (agg k (r* (agg i (r* (b+ C (: i k)) (b+ A (: i j)))) (b+ B (: j k))))))

; TR(ABC) + TR(ADC) = TR(CA(B+D)) 
(define e1 '(r+ (agg i (agg k (r* (agg j (r* (b+ A (: i j)) (b+ B (: j k)))) (b+ C (: k i))))) (agg i (agg k (r* (agg j (r* (b+ A (: i j)) (b+ D (: j k)))) (b+ C (: k i)))))))

(define e1e '(agg i (agg k (r* (b+ C (: k i)) (agg j (r* (b+ A (: j i)) (r+ (b+ B (: k j)) (b+ D (: k j)))))))))

; A^16
(define e2 '(r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A (r* A A)))))))))))))))))

(define ga '(r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                      (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))

(define g0 '(agg a (agg c (r* (r+ (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))
                              (r+ (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))))))

(define g1 '(agg a (agg c (r+ (r+ (r* (b+ x (: a c)) (b+ x (: a c)))
                                  (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))
                              (r+ (r* (b+ x (: a c)) (agg b (r* (b+ u (: a b)) (b+ v (: c b)))))
                                  (r* (agg b (r* (b+ u (: a b)) (b+ v (: c b))))
                                      (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))))))

(define g2a '(r* (agg b (r* (b+ u (: a b)) (b+ v (: c b)))) (agg b (r* (b+ u (: a b)) (b+ v (: c b))))))
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

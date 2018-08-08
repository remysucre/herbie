#lang racket

(require "../common.rkt")
(require "../points.rkt" "../float.rkt")
(require "../alternative.rkt" "../errors.rkt")
(require "../formats/test.rkt")
(require "../formats/datafile.rkt")
(require "../core/matcher.rkt")
(require "../core/regimes.rkt")
(require "../programs.rkt")
(require "../plot.rkt")
(require "../sandbox.rkt")
(require "../formats/tex.rkt")
(require "../fpcore/core2js.rkt")
(require (only-in xml write-xexpr xexpr?))

(provide make-graph make-traceback make-timeout make-axis-plot make-points-plot
         make-plots output-interactive-js make-interactive-js get-interactive-js)

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define/contract (regime-splitpoints altn)
  (-> alt? (listof number?))
  (map sp-point (drop-right (regime-info altn) 1)))

(define/contract (regime-var altn)
  (-> alt? (or/c expr? #f))
  (define info (regime-info altn))
  (and info (sp-bexpr (car info))))

(define/contract (render-command-line)
  (-> string?)
  (format
   "herbie shell --seed ~a ~a"
   (if (vector? (get-seed)) (format "'~a'" (get-seed)) (get-seed))
   (string-join
    (for/list ([rec (changed-flags)])
      (match rec
        [(list 'enabled class flag) (format "+o ~a:~a" class flag)]
        [(list 'disabled class flag) (format "-o ~a:~a" class flag)]))
    " ")))

(define/contract (render-fpcore test)
  (-> test? string?)
  (string-join
   (filter
    identity
    (list
     (format "(FPCore ~a" (test-vars test))
     (format "  :name ~s" (test-name test))
     (if (equal? (test-precondition test) 'TRUE)
         #f
         (format "  :pre ~a" (test-precondition test)))
     (if (equal? (test-expected test) #t)
         #f
         (format "  :herbie-expected ~a" (test-expected test)))
     (if (test-output test)
         (format "\n  :herbie-target\n  ~a\n" (test-output test)) ; Extra newlines for clarity
         #f)
     (format "  ~a)" (test-input test))))
   "\n"))

(define timeline? any/c)

(define/contract (render-timeline timeline)
  (-> timeline? xexpr?)
  `(div ((class "timeline"))
        ,@(for/list ([curr timeline] [next (cdr timeline)])
            `(div
              ((class ,(format "timeline-phase ~a" (dict-ref curr 'type)))
               (data-timespan ,(~a (- (dict-ref next 'time) (dict-ref curr 'time))))
               ,@(for/list ([(type value) (in-dict curr)] #:when (not (member type '(time))))
                   `(,(string->symbol (format "data-~a" type)) ,(~a value))))))))


(define/contract (render-process-info time timeline profile? test #:bug? [bug? #f])
  (->* (number? timeline? boolean? test?) (#:bug? boolean?) xexpr?)
  `(section ((id "process-info"))
    (h1 "Runtime")
    (p ((class "header"))
     "Time bar (total: " (span ((class "number")) ,(format-time time)) ")"
     (a ((class "attachment") (href "debug.txt")) "Debug log")
     ,(if profile?
          `(a ((class "attachment") (href "profile.txt")) "Profile")
          "")
     ,(render-timeline timeline)
     ,(if bug?
          `(p "Please include this information when filing a "
              (a ((href "https://github.com/uwplse/herbie/issues")) "bug report") ":")
          "")
     (pre ((class "shell"))
      (code
       ,(render-command-line) "\n"
       ,(render-fpcore test) "\n")))))

(define (alt2fpcore alt)
  (match-define (list _ args expr) (alt-program alt))
  (list 'FPCore args ':name 'alt expr))

(define (get-interactive-js result)
  (with-handlers ([exn:fail?
                   (λ (e) #f)])
    (define start-fpcore (alt2fpcore (test-result-start-alt result)))
    (define end-fpcore (alt2fpcore (test-result-end-alt result)))
    (define start-js (compile-program start-fpcore #:name "start"))
    (define end-js (compile-program end-fpcore #:name "end"))
    (string-append start-js end-js)))

(define (make-interactive-js result rdir profile?)
  (define js-text (get-interactive-js result))
  (if (string? js-text)
      (display-to-file js-text
                       (build-path rdir "interactive.js")
                       #:exists 'replace)
      #f))

(define (output-interactive-js result rdir profile?)
  (define js-text (get-interactive-js result))
  (if (string? js-text)
      (display js-text)
      #f))

(define/contract (render-interactive start-prog point)
  (-> alt? (listof number?) xexpr?)
  (define start-fpcore (alt2fpcore start-prog))
  `(section ([id "try-it"])
    (h1 "Try it out")
    (div ([id "try-inputs-wrapper"])
     (form ([id "try-inputs"])
      (p ([class "header"]) "Your Program's Arguments")
       (ol
        ,@(for/list ([var-name (second start-fpcore)] [i (in-naturals)] [val point])
            `(li (label ([for ,(string-append "var-name-" (~a i))]) ,(~a var-name))
                 (input ([type "text"]
                         [name ,(string-append "var-name-" (~a i))]
                         [class "input-submit"]
                         [oninput "submit_inputs();"]
                         [value ,(~a val)])))))))
      (div ([id "try-result"] [class "no-error"])
       (p ([class "header"]) "Results")
        (table
         (tbody
           (tr
            (td
             (label ([for "try-original-output"]) "In"))
            (td
             (output ([id "try-original-output"]))))
           (tr
            (td
             (label ([for "try-herbie-output"]) "Out"))
            (td
             (output ([id "try-herbie-output"]))))))
        (div ([id "try-error"]) "Enter valid numbers for all inputs"))))

(define (make-axis-plot result idx out)
  (define var (list-ref (test-vars (test-result-test result)) idx))
  (define split-var? (equal? var (regime-var (test-result-end-alt result))))
  (define pts (test-result-newpoints result))
  (herbie-plot
   #:port out #:kind 'png
   (error-axes pts #:axis idx)
   (map error-mark (if split-var? (regime-splitpoints (test-result-end-alt result)) '()))))

(define (make-points-plot result idx letter out)
  (define-values (theme accessor)
    (match letter
      ['r (values *red-theme*   test-result-start-error)]
      ['g (values *green-theme* test-result-target-error)]
      ['b (values *blue-theme*  test-result-end-error)]))

  (define pts (test-result-newpoints result))
  (define err (accessor result))

  (herbie-plot
   #:port out #:kind 'png
   (error-points err pts #:axis idx #:color theme)
   (error-avg err pts #:axis idx #:color theme)))

(define (make-plots result rdir profile?)
  (define (open-file #:type [type #f] idx fun . args)
    (call-with-output-file (build-path rdir (format "plot-~a~a.png" idx (or type ""))) #:exists 'replace
      (apply curry fun args)))

  (for ([var (test-vars (test-result-test result))] [idx (in-naturals)])
    (when (> (length (remove-duplicates (map (curryr list-ref idx) (test-result-newpoints result)))) 1)
      ;; This is bad code
      (open-file idx make-axis-plot result idx)
      (open-file idx #:type 'r make-points-plot result idx 'r)
      (when (test-result-target-error result)
        (open-file idx #:type 'g make-points-plot result idx 'g))
      (open-file idx #:type 'b make-points-plot result idx 'b))))

(define (make-graph result rdir profile? valid-js-prog)
  (match-define
   (test-result test time bits start-alt end-alt
                points exacts start-est-error end-est-error
                newpoints newexacts start-error end-error target-error timeline)
   result)

   (printf "<!doctype html>\n")
   (write-xexpr
    `(html
      (head
       (meta ([charset "utf-8"]))
       (title "Result for " ,(~a (test-name test)))
       (link ([rel "stylesheet"] [type "text/css"] [href "../graph.css"]))
       ,@js-tex-include
       (script ([src "../report.js"]))
       (script ([src "interactive.js"]))
       (script ([src "https://unpkg.com/mathjs@4.4.2/dist/math.min.js"])))
      (body ([onload "graph()"])

       (section ([id "large"])
        (div "Average Error: "
             (span ([class "number"]
                    [title ,(format "Maximum error: ~a → ~a"
                                    (format-bits (apply max (map ulps->bits start-error)) #:unit #f)
                                    (format-bits (apply max (map ulps->bits end-error)) #:unit #f))])
                   ,(format-bits (errors-score start-error) #:unit #f)
                   " → "
                   ,(format-bits (errors-score end-error) #:unit #f)))
        (div "Time: " (span ([class "number"]) ,(format-time time)))
        (div "Precision: " (span ([class "number"]) ,(format-bits (*bit-width*) #:unit #f)))
        (div "Internal Precision: " (span ([class "number"]) ,(format-bits bits #:unit #f))))

       (section ([id "program"])
        (div ([class "program math"]) "\\[" ,(texify-prog (alt-program start-alt)) "\\]")
        (div ([class "arrow"]) "↓")
        (div ([class "program math"]) "\\[" ,(texify-prog (alt-program end-alt)) "\\]"))

       (section ([id "graphs"])
        (h1 "Error")
        (div
         ,@(for/list ([var (test-vars test)] [idx (in-naturals)])
             (cond
              [(> (length (remove-duplicates (map (curryr list-ref idx) newpoints))) 1)
               (define split-var? (equal? var (regime-var end-alt)))
               (define title "The X axis uses an exponential scale")
               `(figure ([id ,(format "fig-~a" idx)] [class ,(if split-var? "default" "")])
                 (img ([width "800"] [height "300"] [title ,title]
                       [src ,(format "plot-~a.png" idx)]))
                 (img ([width "800"] [height "300"] [title ,title] [data-name "Input"]
                       [src ,(format "plot-~ar.png" idx)]))
                 ,(if target-error
                      `(img ([width "800"] [height "300"] [title ,title] [data-name "Target"]
                             [src ,(format "plot-~ag.png" idx)]))
                      "")
                 (img ([width "800"] [height "300"] [title ,title] [data-name "Result"]
                       [src ,(format "plot-~ab.png" idx)]))
                 (figcaption (p "Bits error versus " (var ,(~a var)))))]
              [else ""]))))

       ,(if valid-js-prog
            (render-interactive start-alt (car points))
            `(p ([display "none"])))

       ,(if (test-output test)
            `(section ([id "comparison"])
              (h1 "Target")
              (table
               (tr (th "Original") (td ,(format-bits (errors-score start-error))))
               (tr (th "Target") (td ,(format-bits (errors-score target-error))))
               (tr (th "Herbie") (td ,(format-bits (errors-score end-error)))))
              (div ([class "math"]) "\\[" ,(texify-prog `(λ ,(test-vars test) ,(test-output test))) "\\]"))
            "")

       (section ([id "history"])
        (h1 "Derivation")
        (ol ([class "history"])
         ,@(render-history end-alt (mk-pcontext newpoints newexacts) (mk-pcontext points exacts))))

       ,(render-process-info time timeline profile? test)))))

(define (make-traceback result rdir profile?)
  (match-define (test-failure test bits exn time timeline) result)
  (printf "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ((charset "utf-8")))
      (title "Exception for " ,(~a (test-name test)))
      (link ((rel "stylesheet") (type "text/css") (href "../graph.css"))))
     (body
      (h1 "Error in " ,(format-time time))
      ,@(cond
         [(exn:fail:user:herbie? exn)
          `((section ([id "user-error"])
             (h2 ,(~a (exn-message exn)) (a ([href ,(herbie-error-url exn)]) " (more)"))
             ,(if (exn:fail:user:herbie:syntax? exn)
                  `(table
                    (thead
                     (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
                    (tbody
                     ,@(for/list ([(stx msg) (in-dict (exn:fail:user:herbie:syntax-locations exn))])
                         `(tr
                           (td ([class "procedure"]) ,(~a msg))
                           (td ,(~a (syntax-source stx)))
                           (td ,(or (~a (syntax-line stx) "")))
                           (td ,(or (~a (syntax-column stx)) (~a (syntax-position stx))))))))
                  "")))]
         [else
          `(,(render-process-info time timeline profile? test #:bug? #t)
            (section ([id "backtrace"])
             (h1 "Backtrace")
             ,(render-traceback exn)))])))))

(define (render-traceback exn)
  `(table
    (thead
     (th ([colspan "2"]) ,(exn-message exn)) (th "L") (th "C"))
    (tbody
     ,@(for/list ([tb (continuation-mark-set->context (exn-continuation-marks exn))])
         (match (cdr tb)
           [(srcloc file line col _ _)
            `(tr
              (td ([class "procedure"]) ,(~a (or (car tb) "(unnamed)")))
              (td ,(~a file))
              (td ,(~a line))
              (td ,(~a col)))]
           [#f
            `(tr
              (td ([class "procedure"]) ,(~a (or (car tb) "(unnamed)")))
              (td ([colspan "3"]) "unknown"))])))))

(define (make-timeout result rdir profile?)
  (match-define (test-timeout test bits time timeline) result)
  (printf "<!doctype html>\n")
  (write-xexpr
   `(html
     (head
      (meta ((charset "utf-8")))
      (title ,(format "Timeout for ~a" (test-name test)))
      (link ([rel "stylesheet"] [type "text/css"] [href "../graph.css"])))
     (body
      (h1 "Timeout in " ,(format-time time))
      (p "Use the " (code "--timeout") " flag to change the timeout.")
      ,(render-process-info time timeline profile? test)))))

(struct interval (alt-idx start-point end-point expr))

(define (interval->string ival)
  (string-join
   (list
    (if (interval-start-point ival)
        (format "~a < " (interval-start-point ival))
        "")
    (~a (interval-expr ival))
    (if (equal? (interval-end-point ival) +nan.0)
        ""
        (format " < ~a" (interval-end-point ival))))))

(define (split-pcontext pcontext splitpoints alts)
  (define preds (splitpoints->point-preds splitpoints alts))

  (for/list ([pred preds])
    (define-values (pts* exs*)
      (for/lists (pts exs)
          ([(pt ex) (in-pcontext pcontext)] #:when (pred pt))
        (values pt ex)))

    ;; TODO: The (if) here just corrects for the possibility that we
    ;; might have sampled new points that include no points in a given
    ;; regime. Instead it would be best to continue sampling until we
    ;; actually have many points in each regime. That would require
    ;; breaking some abstraction boundaries right now so we haven't
    ;; done it yet.
    (if (null? pts*) pcontext (mk-pcontext pts* exs*))))

(define (render-history altn pcontext pcontext2)
  (-> alt? (listof xexpr?))

  (define err
    (format-bits (errors-score (errors (alt-program altn) pcontext))))
  (define err2
    (format "Internal: ~a" (format-bits (errors-score (errors (alt-program altn) pcontext2)))))

  (match altn
    [(alt prog 'start (list))
     (list
      `(li (p "Initial program " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[" ,(texify-prog prog) "\\]")))]
    [(alt prog `(start ,strategy) `(,prev))
     `(,@(render-history prev pcontext pcontext2)
       (li ([class "event"]) "Using strategy " (code ,(~a strategy))))]

    [(alt _ `(regimes ,splitpoints) prevs)
     (define intervals
       (for/list ([start-sp (cons (sp -1 -1 #f) splitpoints)] [end-sp splitpoints])
         (interval (sp-cidx end-sp) (sp-point start-sp) (sp-point end-sp) (sp-bexpr end-sp))))

     `((li ([class "event"]) "Split input into " ,(~a (length prevs)) " regimes")
       (li
        ,@(apply
           append
           (for/list ([entry prevs] [idx (in-naturals)]
                      [new-pcontext (split-pcontext pcontext splitpoints prevs)]
                      [new-pcontext2 (split-pcontext pcontext2 splitpoints prevs)])
             (define entry-ivals (filter (λ (intrvl) (= (interval-alt-idx intrvl) idx)) intervals))
             (define condition (string-join (map interval->string entry-ivals) " or "))
             `((h2 (code "if " (span ([class "condition"]) ,condition)))
               (ol ,@(render-history entry new-pcontext new-pcontext2))))))
       (li ([class "event"]) "Recombined " ,(~a (length prevs)) " regimes into one program."))]

    [(alt prog `(taylor ,pt ,loc) `(,prev))
     `(,@(render-history prev pcontext pcontext2)
       (li (p "Taylor expanded around " ,(~a pt) " " (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog #:loc loc #:color "blue") "\\]")))]

    [(alt prog `(simplify ,loc) `(,prev))
     `(,@(render-history prev pcontext pcontext2)
       (li (p "Simplified" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog #:loc loc #:color "blue") "\\]")))]

    [(alt prog `initial-simplify `(,prev))
     `(,@(render-history prev pcontext pcontext2)
       (li (p "Initial simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog) "\\]")))]

    [(alt prog `final-simplify `(,prev))
     `(,@(render-history prev pcontext pcontext2)
       (li (p "Final simplification" (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog) "\\]")))]

    [(alt prog (list 'change cng) `(,prev))
     `(,@(render-history prev pcontext pcontext2)
       (li (p "Applied " (span ([class "rule"]) ,(~a (rule-name (change-rule cng))))
              (span ([class "error"] [title ,err2]) ,err))
           (div ([class "math"]) "\\[\\leadsto " ,(texify-prog prog #:loc (change-location cng) #:color "blue") "\\]")))]))
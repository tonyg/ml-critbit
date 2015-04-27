#lang racket

(require (only-in srfi/1 delete-duplicates))
(require plot/no-gui)
(require (planet neil/csv))
(require racket/port)

(module asymmetric-error-bars racket
  (require racket/match racket/math racket/list
           plot/utils
           plot/private/common/utils)

  (provide (all-defined-out))

  (define ((asymmetric-error-bars-render-fun xs ys los his color line-width line-style width alpha) area)
    (define clip-rect (send area get-clip-rect))
    (define radius (* 1/2 width))
    (send area put-alpha alpha)
    (send area put-pen color line-width line-style)
    (for ([x  (in-list xs)] [y  (in-list ys)] [lo (in-list los)] [hi (in-list his)])
      (when (rect-contains? clip-rect (vector x y))
        (define v1 (vector x lo))
        (define vm (vector x y))
        (define v2 (vector x hi))
        (send area put-line v1 v2)
        (send area put-tick v1 radius 0)
        (send area put-tick vm radius 0)
        (send area put-tick v2 radius 0)))
    empty)

  (define (asymmetric-error-bars bars
                                 #:x-min [x-min #f] #:x-max [x-max #f]
                                 #:y-min [y-min #f] #:y-max [y-max #f]
                                 #:color [color (error-bar-color)]
                                 #:line-width [line-width (error-bar-line-width)]
                                 #:line-style [line-style (error-bar-line-style)]
                                 #:width [width (error-bar-width)]
                                 #:alpha [alpha (error-bar-alpha)])
    (define fail/kw (make-raise-keyword-error 'asymmetric-error-bars))
    (cond
      [(and x-min (not (rational? x-min)))  (fail/kw "#f or rational" '#:x-min x-min)]
      [(and x-max (not (rational? x-max)))  (fail/kw "#f or rational" '#:x-max x-max)]
      [(and y-min (not (rational? y-min)))  (fail/kw "#f or rational" '#:y-min y-min)]
      [(and y-max (not (rational? y-max)))  (fail/kw "#f or rational" '#:y-max y-max)]
      [(not (rational? line-width))  (fail/kw "rational?" '#:line-width line-width)]
      [(or (> alpha 1) (not (rational? alpha)))  (fail/kw "real in [0,1]" '#:alpha alpha)]
      [else
       (let* ([bars  (sequence->listof-vector 'asymmetric-error-bars bars 4)]
              [bars  (filter vrational? bars)])
         (cond [(empty? bars)  (renderer2d #f #f #f #f)]
               [else
                (match-define (list (vector xs ys los his) ...) bars)
                (let ([x-min  (if x-min x-min (apply min* xs))]
                      [x-max  (if x-max x-max (apply max* xs))]
                      [y-min  (if y-min y-min (apply min* (append ys los his)))]
                      [y-max  (if y-max y-max (apply max* (append ys los his)))])
                  (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                              (asymmetric-error-bars-render-fun xs ys los his
                                                                color line-width line-style
                                                                width alpha)))]))])))

(require 'asymmetric-error-bars)

(define problem-sizes-and-nrepeats
  '((10 30000)
    (20 20000)
    (50 10000)
    (100 5000)
    (200 2500)
    (400 1250)
    (800 625)
    ;; (1000 500)
    ;; (2000 250)
    ;; (4000 125)
    ;; (8000 63)
    ;; (10000 50)
    ;; (20000 25)
    ;; (40000 20)
    ;; (80000 20)
    ;; (100000 20)
    ;; (200000 15)
    ;; (500000 15)
    ;; (1000000 10)
    ))

(define paddings
  (cons 0 (map (lambda (v) (- v 4))
               (list 8
                     16
                     32
                     64
                     128
                     512
                     1024
                     4096
                     16384))))

(define (bases-for-padding padding)
  (delete-duplicates
   (map (lambda (frac) (* padding frac))
        (list 0 1/4 2/4 3/4 4/4))))

(define n-bases (length (bases-for-padding 0)))

;;---------------------------------------------------------------------------
;; Generate and load result CSV

(void (system "make"))
(define results
  (for*/hash ([problem-size-and-nrepeats (in-list problem-sizes-and-nrepeats)]
              [padding (in-list paddings)]
              [base (in-list (bases-for-padding padding))])
    (match-define (list problem-size nrepeats) problem-size-and-nrepeats)
    (define result-filename (format "results-~a-~a-~a-~a.csv" problem-size nrepeats padding base))
    (when (not (file-exists? result-filename))
      (printf "Generating ~a...\n" result-filename)
      (flush-output)
      (system (format "./t.native ~a ~a ~a ~a > ~a"
                      problem-size
                      nrepeats
                      padding
                      base
                      result-filename)))
    (values (list problem-size padding base)
            (let ((rows (csv->list (file->string result-filename))))
              (define headings (car rows))
              (define data-rows (cdr rows))
              (define labels (cdr headings))
              (for/hash ((row data-rows))
                (values (car row)
                        (for/hash ((label labels) (value (cdr row)))
                          (values label value))))))))

;;---------------------------------------------------------------------------
;; Render results

(define variations (list "insertion"
                         "removal"
                         "positive membership"
                         "negative membership"))

(for* ([variation (in-list variations)]
       [padding (in-list paddings)]
       [base (in-list (bases-for-padding padding))])
  (define plot-filename (format "plot-~a-~a-~a.png" variation padding base))
  (printf "Plotting ~a...\n" plot-filename)

  (define (lines-for structure color)
    (lines
     #:color color
     #:label structure
     (for*/list ([problem-size-and-nrepeats (in-list problem-sizes-and-nrepeats)])
       (match-define (list problem-size nrepeats) problem-size-and-nrepeats)
       (define resultset (hash-ref results (list problem-size padding base)))
       (define row-name (format "~a ~a" structure variation))
       (vector problem-size
               (string->number (hash-ref (hash-ref resultset row-name) "mean_rate_kHz"))))))

  (define (error-bars-for structure)
    (asymmetric-error-bars
     (for*/list ([problem-size-and-nrepeats (in-list problem-sizes-and-nrepeats)])
       (match-define (list problem-size nrepeats) problem-size-and-nrepeats)
       (define resultset (hash-ref results (list problem-size padding base)))
       (define row-name (format "~a ~a" structure variation))
       (vector problem-size
               (string->number (hash-ref (hash-ref resultset row-name) "mean_rate_kHz"))
               (string->number (hash-ref (hash-ref resultset row-name) "lo_95ci_rate_kHz"))
               (string->number (hash-ref (hash-ref resultset row-name) "hi_95ci_kHz")))))) ;; TODO: fix

  (parameterize ([discrete-histogram-skip 2.5] ;; any value larger than the number of alternatives
                 [plot-x-transform log-transform])
    (plot-file (list (lines-for "Critbit" 1)
                     (lines-for "StringSet" 2)
                     (error-bars-for "Critbit")
                     (error-bars-for "StringSet"))
               plot-filename
               'png
               #:title (format "Item ~a, item length ~a, common prefix ~a"
                               variation
                               padding
                               base)
               #:x-label "Set size"
               #:y-label "Operations per second, thousands (kHz)"
               #:legend-anchor 'top-right
               #:y-min 0)))
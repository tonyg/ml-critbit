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

(define problem-sizes-bulk-counts-and-nrepeats
  '(
    (10 500 300)
    (20 250 400)
    (50 100 500)
    (100 10 500)
    (200 5 500)
    (400 2 625)
    (800 2 625)
    (1000 2 500)
    (2000 2 250)
    (4000 1 125)
    (8000 1 63)
    (10000 1 50)
    (20000 1 25)
    (40000 1 20)
    (80000 1 20)
    (100000 1 20)
    (200000 1 15)
    ;; (500000 1 15)
    ;; (1000000 1 10)
    ))

(define paddings
  (cons 0 (map (lambda (v) (- (expt 2 v) 4))
               (list
                3
                4
                6
                8
                10
                12
                14
                ))))

(define (bases-for-padding padding)
  (delete-duplicates
   (map (lambda (frac) (* padding frac))
        (list
         0
         ;; 1/4
         2/4
         ;; 3/4
         4/4
         ))))

(define n-bases (length (bases-for-padding 0)))

(define (too-big? problem-size padding)
  (> (* problem-size padding) 8000000000))

;;---------------------------------------------------------------------------
;; Generate and load result CSV

(void (system "make"))

(define (generate-results problem-size nrepeats bulk-count padding base)
  (define result-filename
    (format "results-~a-~a-~a-~a-~a.csv" problem-size nrepeats bulk-count padding base))
  (when (not (file-exists? result-filename))
    (printf "Generating ~a...\n" result-filename)
    (flush-output)
    (system (format "./t.native ~a ~a ~a ~a ~a > ~a"
                    problem-size
                    nrepeats
                    bulk-count
                    padding
                    base
                    result-filename))))

(define gen-ch (make-channel))
(define done-ch (make-channel))
(define n-workers 4)

(for ([n n-workers])
  (thread (lambda ()
            (let loop ()
              (define params (channel-get gen-ch))
              (when params
                (log-info "Worker ~v running ~v" n params)
                (apply generate-results params)
                (loop)))
            (channel-put done-ch #t))))

(for* ([size-count-and-nrepeats (in-list problem-sizes-bulk-counts-and-nrepeats)]
       [padding (in-list paddings)]
       [base (in-list (bases-for-padding padding))]
       #:when (not (too-big? (car size-count-and-nrepeats) padding)))
  (match-define (list problem-size bulk-count nrepeats) size-count-and-nrepeats)
  (channel-put gen-ch (list problem-size nrepeats bulk-count padding base)))

(for ([n n-workers])
  (channel-put gen-ch #f)
  (channel-get done-ch))

(define results
  (for*/hash ([size-count-and-nrepeats (in-list problem-sizes-bulk-counts-and-nrepeats)]
              [padding (in-list paddings)]
              [base (in-list (bases-for-padding padding))]
              #:when (not (too-big? (car size-count-and-nrepeats) padding)))
    (match-define (list problem-size bulk-count nrepeats) size-count-and-nrepeats)
    (define result-filename
      (format "results-~a-~a-~a-~a-~a.csv" problem-size nrepeats bulk-count padding base))
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

  (define (lines-for structure
                     color
                     #:suffix [suffix ""]
                     #:key [key "mean_rate_kHz"]
                     #:style [style 'solid])
    (lines
     #:color color
     #:label (string-append structure suffix)
     #:style style
     (for*/list ([size-count-and-nrepeats (in-list problem-sizes-bulk-counts-and-nrepeats)]
                 #:when (not (too-big? (car size-count-and-nrepeats) padding)))
       (define problem-size (car size-count-and-nrepeats))
       (define resultset (hash-ref results (list problem-size padding base)))
       (define row-name (format "~a ~a" structure variation))
       (vector problem-size
               (string->number (hash-ref (hash-ref resultset row-name) key))))))

  (define (raw-data-for structure color)
    (points
     #:color color
     #:sym 'fullcircle4
     #:alpha 0.05
     (for/fold ([points '()])
               ([size-count-and-nrepeats (in-list problem-sizes-bulk-counts-and-nrepeats)]
                #:when (not (too-big? (car size-count-and-nrepeats) padding)))
       (define problem-size (car size-count-and-nrepeats))
       (define resultset (hash-ref results (list problem-size padding base)))
       (define row-name (format "~a ~a" structure variation))
       (define rawdata (string-split (hash-ref (hash-ref resultset row-name) "rawdata")))
       (for/fold ([points points])
                 ([datum rawdata])
         (cons (vector problem-size (string->number datum)) points)))))

  (define (error-bars-for structure)
    (asymmetric-error-bars
     (for*/list ([size-count-and-nrepeats (in-list problem-sizes-bulk-counts-and-nrepeats)]
                 #:when (not (too-big? (car size-count-and-nrepeats) padding)))
       (define problem-size (car size-count-and-nrepeats))
       (define resultset (hash-ref results (list problem-size padding base)))
       (define row-name (format "~a ~a" structure variation))
       (vector problem-size
               (or (string->number (hash-ref (hash-ref resultset row-name) "mean_rate_kHz")) 0)
               (or (string->number (hash-ref (hash-ref resultset row-name) "lo_95ci_rate_kHz")) 0)
               (or (string->number (hash-ref (hash-ref resultset row-name) "hi_95ci_rate_kHz")) +inf.0)))))

  (define (max-min-for structure sym)
    (points
     #:sym sym
     (for*/fold ([ps '()])
                ([size-count-and-nrepeats (in-list problem-sizes-bulk-counts-and-nrepeats)]
                 #:when (not (too-big? (car size-count-and-nrepeats) padding)))
       (define problem-size (car size-count-and-nrepeats))
       (define resultset (hash-ref results (list problem-size padding base)))
       (define row-name (format "~a ~a" structure variation))
       (define min-rate (string->number (hash-ref (hash-ref resultset row-name) "q0_rate_kHz")))
       (define max-rate (string->number (hash-ref (hash-ref resultset row-name) "q4_rate_kHz")))
       (append (if min-rate (list (vector problem-size min-rate)) '())
               (if max-rate (list (vector problem-size max-rate)) '())
               ps))))

  (parameterize ([discrete-histogram-skip 2.5] ;; any value larger than the number of alternatives
                 [plot-x-transform log-transform]
                 [plot-x-ticks (log-ticks)])
    (plot-file (list (lines-for #:suffix " (mean)" "Critbit" 1)
                     (lines-for #:suffix " (mean)" "StringSet" 2)
                     (lines-for #:suffix " (mean)" "Hashtbl" 3)
                     (lines-for #:suffix " (median)"
                                #:key "q2_rate_kHz" #:style 'short-dash "Critbit" 1)
                     (lines-for #:suffix " (median)"
                                #:key "q2_rate_kHz" #:style 'short-dash "StringSet" 2)
                     (lines-for #:suffix " (median)"
                                #:key "q2_rate_kHz" #:style 'short-dash "Hashtbl" 3)
                     (raw-data-for "Critbit" 1)
                     (raw-data-for "StringSet" 2)
                     (raw-data-for "Hashtbl" 3)
                     (error-bars-for "Critbit")
                     (error-bars-for "StringSet")
                     (error-bars-for "Hashtbl")
                     ;; (max-min-for "Critbit" 1)
                     ;; (max-min-for "StringSet" 2)
                     ;; (max-min-for "Hashtbl" 3)
                     )
               plot-filename
               'png
               #:width 640
               #:height 480
               #:title (format "Item ~a, item length ~a, common prefix ~a"
                               variation
                               (+ padding 4)
                               base)
               #:x-label "Set size"
               #:y-label "Operations per second, thousands (kHz)"
               #:legend-anchor 'top-right
               #:x-min 9
               #:y-min 0)))

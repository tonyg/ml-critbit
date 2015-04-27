#lang racket

(require (only-in srfi/1 delete-duplicates))

(void (system "make"))
(for* ([problem-size-and-nrepeats (in-list '((10 30000)
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
                                             ))]
       [padding (in-list (cons 0 (map (lambda (v) (- v 4))
                                      (list 8
                                            16
                                            32
                                            64
                                            128
                                            512
                                            1024
                                            4096
                                            16384))))]
       [base (in-list (delete-duplicates
                       (map (lambda (frac) (* padding frac))
                            (list 0 1/4 2/4 3/4 4/4))))])
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
  )

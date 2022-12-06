#lang racket/base

(require racket/contract
         racket/match
         (only-in racket/base
                  [inexact->exact exact])
         (only-in racket/function
                  const)
         (only-in racket/sequence
                  in-slice)
         (only-in racket/stream
                  [stream-map map]))

(provide (contract-out
           [braille-sparklines (-> (listof real?) positive? string?)]
           [braille-hires-sparklines (-> (listof real?) positive? string?)]
           [bar-sparklines (-> (listof real?) positive? string?)]))

(define braille-offset #x2800)

(define braille-bitpatterns
  #(
    ;; left column
    #b00000000 ;; 0
    #b01000000 ;; 1
    #b01000100 ;; 2
    #b01000110 ;; 3
    #b01000111 ;; 4
    ;; right column
    #b00000000 ;; 0
    #b10000000 ;; 1
    #b10100000 ;; 2
    #b10110000 ;; 3
    #b10111000 ;; 4
    ))

(define braille-hires-bitpatterns
  #(
    ;; left column
    #b00000000 ;; 0
    #b01000000 ;; 1
    #b01000100 ;; 2
    #b01000110 ;; 3
    #b01000111 ;; 4
    #b00000111 ;; 5
    #b00000011 ;; 6
    #b00000001 ;; 7
    ;; right column
    #b00000000 ;; 0
    #b10000000 ;; 1
    #b10100000 ;; 2
    #b10110000 ;; 3
    #b10111000 ;; 4
    #b00111000 ;; 5
    #b00011000 ;; 6
    #b00001000 ;; 7
    ))

(define (braille-sparklines data maxv)
  (braille-sparklines/pattern braille-bitpatterns 5 4 data maxv))

(define (braille-hires-sparklines data maxv)
  (braille-sparklines/pattern braille-hires-bitpatterns 8 7 data maxv))

(define (braille-sparklines/pattern pattern off norm-max data maxv)
  (define norm
    (map (lambda (x) (min norm-max (exact (round (* norm-max (/ x maxv)))))) data))

  (list->string
    (for/list ([p (in-slice 2 norm)])
      (match-define (or (list a b)
                        (and (list a) (app (const 0) b))) p)
      (define apat (vector-ref pattern a))
      (define bpat (vector-ref pattern (+ off b)))
      (integer->char (+ braille-offset (bitwise-ior apat bpat))))))

(define bars
  (list->vector
    (cons
      #\space
      (for/list ([c (in-range 8)])
        (integer->char (+ #x2581 c))))))

(define (bar-sparklines data maxv)
  (define norm
    (map (lambda (x) (min 8 (exact (round (* 8 (/ x maxv)))))) data))
  (list->string
    (for/list ([d norm])
      (vector-ref bars d))))

(module* main #f
  (define data (let ([a (for/list ([x 8]) x)]) (append a (reverse a))))
  (displayln (braille-sparklines data (apply max data))) 
  (displayln (braille-hires-sparklines data (apply max data))) 
  (displayln (bar-sparklines data (apply max data))))
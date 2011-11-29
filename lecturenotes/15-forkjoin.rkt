#lang plai
;
;These are lecture notes for the "Programming Languages and Types" course by Klaus Ostermann 
;at the University of Marburg
;
;loosely based on Sec. 19 of "Programming Languages: Application and Interpretation" by
;Shriram Krishnamurthi
;
;Please comment/correct/improve these notes via github. Proposals or questions can
;be submitted as an "issue"; proposals for corrections/extensions/improvements can
;be submitted as a "pull request". You can of course also send an email to Klaus Ostermann 

; Racket is a language with so-called _first-class continuations_. It can reify the 
; current continuation automatically and on the fly. As you may imagine, creating a
; continuation involves copying the stack, but there are less and more efficient ways of
; obtaining the same effect.

; Adding continuations to a language makes it easy to create a better web programming protocol,
; as we shall see. But first-class continuations are much more general and give programmers
; immense power in numerous contexts.

; In Racket (and the related programming language Scheme), a continuation is created with
; let/cc. It can be used to give the current continuation a name: (let/cc k ... k ...)

; Let's write some programs using continuations (try this in the Racket read-eval-print loop).

; (let/cc k (k 3))  ; what is the continuation k here?

; (+ 1 (let/cc k (k 3))) ; what is the continuation k here?

; using let/cc for exception handling: let/cc acts as the "try", invoking k as the "throw".
;  (define (f n) (+ 10 (* 5 (let/cc k (/ 1 (if (zero? n) (k 1) n))))))

; Let's now consider a more sophisticated usage of let/cc, namely to program a simple form
; of cooperative multi-threading, often called _co-routines_. A co-routine designates points
; in the routine where a switch to another routine should occur - a so-called yield point.
; With let/cc we can program co-routines within the language, without having any dedicated
; built-in support for it:

(define queue empty)

(define (empty-queue?)
  (empty? queue))

(define (enqueue x)
  (set! queue (append queue (list x))))

(define (dequeue)
  (let ((x (first queue)))
    (set! queue (rest queue))
    x))

(define (fork) 
  (let/cc k
    (begin
      (enqueue (lambda () (k 1))) ; enqueue thunk
      0)))

(define (join)
  (if (not (empty-queue?)) 
      ((dequeue))
      'alljoined))

(define (yield)
  (let/cc k
    (enqueue k)
    ((dequeue))))  ; invoke thunk

(define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
(define (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(define (printfibs n)
  (if (zero? n)
      (begin (print "Fertig mit fibs") (newline))
      (begin
        (print (format "Fib(~A)=~A" n (fib n)))
        (newline)
        (yield)
        (printfibs (- n 1)))))

(define (printfacts n)
  (if (zero? n)
      (begin (print "Fertig mit facts") (newline))
      (begin
        (print (format "Fact(~A)=~A" n (fact n)))
        (newline)
        (yield)
        (printfacts (- n 1)))))


(if (= (fork) 0) 
    (printfibs 8)
    (printfacts 12))

(join)

(if (= (fork) 0) 
    (printfibs 10)
    (printfacts 8))

(join)

#lang racket
; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2019                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains simple dictionary definition
;;(require "dictionary.rkt")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***
(define k 5413)
(define hashed 0)
(define final 0)
(define keyword-key 0)
(define checker (cons 1 null))
(define counter 0)
(define lister (list 1 2 3))

(define x
  (lambda (hashes hashfunctionlist l)
    ;;(display hashes)
    ;;(cond (null? (rest hashfunctionlist) (f hashes ((car hashfunctionlist) l)))
    (cond ((null? hashfunctionlist) #t)
          (else (and (f hashes ((car hashfunctionlist) l)) (x hashes (cdr hashfunctionlist) l))
))))
(define f
  
  (lambda (hashes hash)
    ;;(display (rest hashes))
    (cond ((null? (rest hashes)) (= (car hashes) hash))
          (else (or (= (car hashes) hash) (f (rest hashes) hash)))
)))

(define keyer
  (lambda (hashfunctionlist dict l)
(cond ((null? hashfunctionlist) 0)
      (else
       ;;(set! counter (+ counter 1))
        ;;(set! checker (append checker ((car hashfunctionlist) l)))
       (set! checker (cons checker ((car hashfunctionlist) l))) 
         (keyer (cdr hashfunctionlist) dict l)))
      checker

    ))

(define bitvector (cons 0 null))

(define bit-maker
  (lambda (hashfunctionlist dict)
    (cond ((null? (cdr hashfunctionlist)) (cons (bitvector-maker (car hashfunctionlist) dict) '()))
          (else
           
           ;;(set! counter (+ counter 1))
           (cons (bitvector-maker (car hashfunctionlist) dict) (bit-maker (cdr hashfunctionlist) dict)))))
           
          )
          
    

(define bitvector-maker
  (lambda (hashfunctionlist dict)
    ;;(hashfunctionlist (car dict))

(cond ((null? (cdr dict)) (cons (hashfunctionlist (car dict)) '()))
      (else
    (define l (car dict))
    (cons (hashfunctionlist (car dict)) (bitvector-maker hashfunctionlist (cdr dict))
    
    )))
    )

        
    )


;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
     (if (null? (cdr w)) (+ (* 29 k) (ctv (car w))) (+ (* 29 (key(cdr w))) (ctv (car w))))
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 111037761665
;;   (key '(m a y))           = 132038724
;;   (key '(t r e e f r o g)) = 2707963878412931

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size)
    (lambda (l)
      (remainder (key l) size))
))



;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size)
     (lambda (w)
       (floor (* size (- (* (key w) A) (floor (* (key w) A)))))
       )
))


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

;;(define hash-1 (gen-hash-division-method 70111))
;;(define hash-2 (gen-hash-division-method 89989))
;;(define hash-3 (gen-hash-multiplication-method 700426))
;;(define hash-4 (gen-hash-multiplication-method 952))

;;(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
;;(define hashfl-2 (list hash-1 hash-3))
;;(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;; (hash-1 '(h e l l o))        ==> 26303
;; (hash-1 '(m a y))            ==> 19711
;; (hash-1 '(t r e e f r o g))  ==> 3010
;;
;; (hash-2 '(h e l l o))        ==> 64598
;; (hash-2 '(m a y))            ==> 24861
;; (hash-2 '(t r e e f r o g))  ==> 23090
;;
;; (hash-3 '(h e l l o))        ==> 313800.0
;; (hash-3 '(m a y))            ==> 317136.0
;; (hash-3 '(t r e e f r o g))  ==> 525319.0
;;
;; (hash-4 '(h e l l o))        ==> 426.0
;; (hash-4 '(m a y))            ==> 431.0
;; (hash-4 '(t r e e f r o g))  ==> 714.0

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR


(define gen-checker
  (lambda (hashfunctionlist dict)
    
    
    (lambda (l) 
          ;;((car hashfunctionlist) l)
      ;;(set! counter (+ counter 1))
      (reduce + lister 1) 
      (x (flatten (bit-maker hashfunctionlist dict)) hashfunctionlist l)
      ;;(set! bitvector (cons bitvector null))
    ;;(display counter)
      ;;(tfdetector bitvector checker)
      ;;(display checker)
      ;;(display bitvector)
      
      )
    
    
))



;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

;;(define checker-1 (gen-checker hashfl-1 dictionary))
;;(define checker-2 (gen-checker hashfl-2 dictionary))
;;(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #f

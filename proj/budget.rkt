#lang racket

(require csv-reading)

;; CONSTANTS
(define VENDOR_IDX 2)
(define CATEGORIES
  '("food"
    "transportation"
    "books"
    "home"
    "person"
    "other"))
(define VENDOR_CATEGORIES (make-hash))

;; PARSING
(define (reader in)
  (make-csv-reader in))

(define rows
  (reader (open-input-file "data.csv")))

(define (extract-vendors)
  (csv-map
   (lambda (row) (list-ref row VENDOR_IDX)) rows))

(define (get-unique-vendors vendors)
  (cond [(empty? vendors) vendors]
        [(member (first vendors) (rest vendors))
         (get-unique-vendors (rest vendors))]
        [else
         (cons (first vendors)
               (get-unique-vendors (rest vendors)))]))

;; CATEGORIZING
(define (print-options opts)
  (for ([idx (in-range (length opts))]
        [cat opts])
    (display (string-append
              (number->string idx)
              " - "
              cat
              "\n"))))

(define (get-category vendor)
  (display
   (string-append
    "Select category for vendor "
    vendor
    "\n"))
  (print-options CATEGORIES)
  (let ([idx (string->number (read-line))])
    (if (and (< idx (length CATEGORIES)) (>= idx 0))
        (list-ref CATEGORIES idx)
        (get-category vendor))))

(for-each
 (lambda (vendor)
   (let ([cat (get-category vendor)])
     (hash-set! VENDOR_CATEGORIES vendor cat)))
;; (get-unique-vendors (extract-vendors))
 '("a" "b" "c")
 )

;; write V_C to database
;; make script with cmd line option for file...
;; or make csv reader read from std in and just use
;; shell redirects

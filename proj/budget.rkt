#lang racket

(require csv-reading
         db)

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

;; HELPERS
(define (intersperse a xs)
  (cond [(empty? xs) xs]
        [(empty? (rest xs)) xs]
        [else
         (cons (first xs)
               (cons a
                (intersperse a (rest xs))))]))

(define (list->sqlenum xs)
  (string-append
   "("
   (foldl
    (lambda (wrd acc)
      (string-append wrd acc))
    ""
    (intersperse
     ", "
     (map
      (lambda (x)
        (string-append "'" x "'"))
      xs)))
   ")"))

;; DATABASE
(define pgdb
  (postgresql-connect #:user "branweb"
                      #:database "budget"))

(define bootstrap?
  (not (or (table-exists? pgdb "vendors")
           (table-exists? pgdb "transactions"))))

(define (bootstrap-tables)
  (when bootstrap?
    (let ([cat-query 
           (string-append
            "CREATE TYPE category AS ENUM "
            (list->sqlenum CATEGORIES))]
          [vendors-query
           (string-append
            "CREATE TABLE vendors ("
            "id SERIAL PRIMARY KEY,"
            "name varchar(255) NOT NULL,"
            "category category NOT NULL)")]
          [transactions-query
           (string-append
            "CREATE TABLE transactions ("
            "id SERIAL PRIMARY KEY,"
            "trans_amt numeric(6, 4) NOT NULL,"
            "trans_date date NOT NULL,"
            "vendor_id integer REFERENCES vendors(id))")])
      (query-exec pgdb cat-query)
      (query-exec pgdb vendors-query)
      (query-exec pgdb transactions-query))))

(define (insert-vendors)
  (hash-for-each
   VENDOR_CATEGORIES
   (lambda (vendor cat)
     (query-exec
      pgdb
      "INSERT INTO vendors (name, category) VALUES ($1, cast($2 as category))"
      vendor
      cat))))

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

(define (categorize-vendors)
  (for-each
   (lambda (vendor)
     (let ([cat (get-category vendor)])
       (hash-set! VENDOR_CATEGORIES vendor cat)))
;;   (get-unique-vendors (extract-vendors))
   '("a" "b" "c")
   ))



;; loads csv file
;; makes set of vendors therein
;; prompts user to categorize each vendor
;; builds a hash (VENDOR => CATEGORY) e.g. Chick-fil-a => food

;; needs to query db for each vendor
;; if unknown, then prompt user and add to hash
;; otherwise don't prompt
;; when hash is build, use it to update the database (insert into vendors values (name, cat))
;; should either generate a report or add budget info to db
;; (for-each
;;  (lambda (vendor)
;;    (let ([cat (get-category vendor)])
;;      (hash-set! VENDOR_CATEGORIES vendor cat)))
;; ;; (get-unique-vendors (extract-vendors))
;;  '("a" "b" "c")
;;  )

;; write V_C to database
;; make script with cmd line option for file...
;; or make csv reader read from std in and just use
;; shell redirects


(define (main)
  (bootstrap-tables)
  (categorize-vendors)
  (insert-vendors)
;;  (print VENDOR_CATEGORIES)
  )

(main)



#lang racket

(require csv-reading
         db
         racket/hash)

;; Takes a csv of credit card transactions

;; Loads all vendors in the db into an in-memory hash table

;; Compares vendors in csv against those in the hash table

;; If it alread knows about the vendor, it does not prompt the user

;; If it finds a vendor it doesn't know about, it prompts the user
;; to assign the vendor a predefined category. This is beceause
;; the cc company is bad at categorizing vendors. It collects the
;; users choices in a separate hash table

;; It then writes the user-assigned categories in the hash table
;; back to the db.

;; It combines the original table with the new one. This combo should
;; reflect the state of the db vendors table

;; It goes through transactions in csv file, looking up category in the
;; hash table.

;; It writes the transaction with the looked-up category to the db

;; TODO: allow it to take file as cmd line arg
;; TODO: figure out how to ensure we don't record duplicate transactions
;; TODO: how to categorize generic vendors like amazon
;; TODO: split out recurring expenses (could add a flag to the db table)

;; HELPERS
(define (format-amt n)
  (* -1 (string->number n)))

;; CONSTANTS
(define VENDOR_IDX 2)
(define CATEGORIES
  '("food"
    "transportation"
    "learning"
    "home"
    "recurring"
    "other"))

(define VENDOR_CATEGORIES (make-hash))

;; DATABASE
(define pgdb
  (postgresql-connect #:user "branweb"
                      #:database "budget"))

(define bootstrap?
  (ormap
   (lambda (t) (not (table-exists? pgdb t)))
   (list "vendors" "categories" "transactions")))

(define (create-tables)
  (let ([cat-query 
         (string-append
          "CREATE TABLE categories("
          "id SERIAL PRIMARY KEY,"
          "name varchar(255) NOT NULL)")]
        [vendors-query
         (string-append
          "CREATE TABLE vendors ("
          "id SERIAL PRIMARY KEY,"
          "name varchar(255) NOT NULL,"
          "category_id integer REFERENCES categories(id))")]
        [transactions-query
         (string-append
          "CREATE TABLE transactions ("
          "id SERIAL PRIMARY KEY,"
          "trans_amt numeric(6, 4) NOT NULL,"
          "trans_date date NOT NULL,"
          "vendor_id integer REFERENCES vendors(id))")])
    (query-exec pgdb cat-query)
    (query-exec pgdb vendors-query)
    (query-exec pgdb transactions-query)))

(define (insert-categories)
  (for-each
   (lambda (cat)
     (query-exec
      pgdb
      "INSERT INTO categories (name) VALUES ($1)" cat))
   CATEGORIES))

(define (bootstrap-db)
  (when bootstrap?
    (display "Bootstrapping database...\n")
    (create-tables)
    (insert-categories)))

(define (insert-vendors)
  (hash-for-each
   VENDOR_CATEGORIES
   (lambda (vendor cat)
     (query-exec
      pgdb
      "INSERT INTO vendors (name, category_id) VALUES ($1, $2)"
      vendor
      (query-value pgdb "SELECT id FROM categories WHERE name = $1" cat)))))

(define existing-vendor-categories
  (make-hash
   (map
    (lambda (row)
      (cons (vector-ref row 0)
            (vector-ref row 1)))
    (query-rows
     pgdb
     (string-append "SELECT v.name, c.name FROM vendors v "
                    "JOIN categories c ON c.id = v.category_id")))))

(define known-vendors
  (hash-keys existing-vendor-categories))

;; PARSING
(define (reader in)
  (make-csv-reader in))

;; convert rows to list of lists and remove headers
(define rows
  (rest (csv->list (reader (open-input-file "data-new.csv")))))

;; payments/credits show as postive amounts so filter them out
(define (filter-payments xxs)
  (filter
   (lambda (xs)
     (negative? (string->number (list-ref xs 5))))
   xxs))

(define (filter-category xxs cat)
  (filter
   (lambda (xs)
     (not (equal?
           cat
           (hash-ref
            existing-vendor-categories
            (list-ref xs VENDOR_IDX)))))
   xxs))

(define (filter-known-vendors vendors)
  (filter
   (lambda (vendor)
     (not (member vendor known-vendors)))
   vendors))

(define (extract-vendors)
  (filter-known-vendors
   (map
    (lambda (row)
      (list-ref row VENDOR_IDX))
    (filter-payments rows))))

(define (get-unique-vendors vendors)
  (cond [(empty? vendors) vendors]
        [(member (first vendors) (rest vendors))
         (get-unique-vendors (rest vendors))]
        [else
         (cons (first vendors)
               (get-unique-vendors (rest vendors)))]))

;; CATEGORIZING
(define (print-options opts)
  (for ([idx (in-range 1 (add1 (length opts)))]
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
  (let ([idx (sub1 (string->number (read-line)))])
    (if (and (< idx (length CATEGORIES)) (>= idx 0))
        (list-ref CATEGORIES idx)
        (get-category vendor))))

(define (categorize-vendors)
  (for-each
   (lambda (vendor)
     (let ([cat (get-category vendor)])
       (hash-set! VENDOR_CATEGORIES vendor cat)))
   (get-unique-vendors (extract-vendors))))

;; RECORDING TRANSACTIONS
(define (write-transactions)
  (for-each
   (lambda (row)
     (let* ([vendor (list-ref row 2)]
            [category (hash-ref existing-vendor-categories vendor)]
            [amt (format-amt (list-ref row 5))]
            [post-date (list-ref row 1)])
       (query-exec
        pgdb
        (string-append
         "INSERT INTO transactions "
         "(trans_amt, trans_date, vendor_id) "
         "VALUES ($1, to_date($2, 'MM/DD/YYYY'), $3)")
        amt
        post-date
        (query-value
         pgdb
         "SELECT id FROM vendors WHERE name = $1"
         vendor))))
   (filter-category (filter-payments rows) "recurring")))

;; MAIN
(define (main)
  (bootstrap-db)
  (categorize-vendors)
  (insert-vendors)
  (hash-union! existing-vendor-categories VENDOR_CATEGORIES)
  (write-transactions))

(main)




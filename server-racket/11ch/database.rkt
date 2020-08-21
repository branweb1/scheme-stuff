#lang racket

(require db)

(define user (getenv "DB_USER"))
(define database (getenv "DB_NAME"))

;; FOR WEBSERVER / MULTITHREADED APP
;; (define db-source
;;   (postgresql-data-source
;;    #:user user
;;    #:port 5432
;;    #:server "localhost"
;;    #:database database))

;; (define pgdb
;;   (virtual-connection
;;    (connection-pool
;;     (lambda () (dsn-connect db-source))
;;     #:max-connections 10
;;     #:max-idle-connections 10)))

;; (query-rows pgdb "select * from students where last_name like 'W%' and id = $1" 90)

;; SINGLE THREADED BATCH APP
(define pgdb
  (postgresql-connect
   #:user user
   #:port 5432
   #:server "localhost"
   #:database database))

(query-rows pgdb "select * from students where last_name like 'W%' and id = $1" 90)

(disconnect pgdb)



#lang racket

(require web-server/http/request-structs)
(require web-server/http/response-structs)
(require web-server/servlet-env)
(require (file "../sample-code/respond.rkt"))

;; struct-copy :: <struct name> <struct-to-copy> <fields-to-change>
;; copies struct-to-copy overwriting specified fields
;; (struct meal [main side] #:transparent)
;; (define fast (meal "burger" '("fries" "cola")))
;; (define second (struct-copy meal fast [main "chicken fingers"]))

(define (head->get req)
  (struct-copy request
               req
               [method "#GET"]))

(define (strip-body resp)
  (struct-copy response
               resp
               [output write-nothing]))

(define (write-nothing port)
  (write-bytes #"" port))

(define (start req)
  (if (bytes=? #"HEAD" (request-method req))
      (strip-body (dispatcher (head->get req)))
      (dispatcher req)))

(define (dispatcher req)
  (respond/text #:body "hello\n"))

(serve/servlet
 dispatcher
 #:port 6995
 #:launch-browser? #f
 #:servlet-regexp #rx"")

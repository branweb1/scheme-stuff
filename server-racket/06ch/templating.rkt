#lang racket

(require web-server/servlet-env
	 web-server/templates
         web-server/http/request-structs
         web-server/dispatch
         (file "../sample-code/respond.rkt"))

(define (greet-person req name)
  (respond/html
   #:body (include-template "hello.html")))

(define-values (dispatcher url-generator)
  (dispatch-rules
   [("hello" (string-arg)) greet-person]))

(serve/servlet
 dispatcher
 #:port 6995
 #:command-line? #t
 #:launch-browser? #f
 #:servlet-regexp #rx"")

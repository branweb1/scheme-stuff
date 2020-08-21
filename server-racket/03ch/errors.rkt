#lang racket

(require web-server/servlet-env
	 web-server/templates
         web-server/http/request-structs
	 net/url-string
         web-server/dispatch
	 gregor
         (file "../sample-code/respond.rkt"))

(define-values (dispatcher url-generator)
  (dispatch-rules
   [[(integer-arg)] #:method "get" get-homepage])) ;; route fails to match if given non-int


;; template : anything in function scope referencd with @|<var>| inside template....maybe there are some programmatic things you can do there too?
(define (get-homepage req n)
  (let ([r (/ 1 n)])
    (respond/html
     #:body (include-template "rational.html"))))

(define (oops url ex)
  (log-error "[~a] ~a ~a"
             (datetime->iso8601 (now/utc)) ;; from gregor
             (url->string url)
             (exn-message ex))
  (respond/html
   #:code 500
   #:body (include-template "oops.html")))

(define (not-found req)
  (let* ([url (request-uri req)]
         [body (format "~a \n Quoth the server:404"
                       (url->string url))])
    (respond #:code 404
             #:mime "text/plain"
             #:body body)))

;;  #:file-not-found-responder - a kind of else route. If we could not match a route, fall back to this
;;  #:servlet-responder - if a route matched but there was an error in the servlet assigned to handle the request.

(serve/servlet
 dispatcher
 #:port 6893
 #:command-line? #t
 #:servlet-responder oops ;; server error handler
 #:file-not-found-responder not-found ;; 404
 #:launch-browser? #f
 #:servlet-regexp #rx"")

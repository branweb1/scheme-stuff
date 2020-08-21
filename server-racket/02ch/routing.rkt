#lang racket

(require web-server/servlet-env
         web-server/dispatch
         (file "../sample-code/respond.rkt"))

;; AUX
(define greetings
  (hash "en" "hello"
        "de" "hallo"
        "es" "hola"
        "fr" "bonjour"))

(define languages (hash-keys greetings))

(define lang-num (length languages))

(define (random-language)
  (list-ref languages (random lang-num)))

;; SERVLETS
(define (hello req)
  (let* ([lang (random-language)]
         [greeting (hash-ref greetings lang)])
    (set-location (respond/text #:body greeting)
                  (url-generator hello+lang lang)))) ;; using dispatch-rules generated route contructor function

(define (hello+lang req lang)
  (let ([greeting (hash-ref greetings lang #f)])
    (if (string? greeting)
        (respond/text #:body greeting)
        (not-found req))))

(define (method-not-allowed req)
  (respond #:code 405))

(define (not-found req)
  (respond #:code 404))

(define-values (dispatcher url-generator)
  (dispatch-rules
   [("hello") hello]
   [("hello" (string-arg)) hello+lang] ;; parse arg from url
   [("hello") #:method (regexp ".*") method-not-allowed]
   [else not-found]))


(serve/servlet
 dispatcher
 #:port 6995
 #:launch-browser? #f
 #:servlet-regexp #rx"")

#lang racket

(require json)


;; parsing: json -> racket jsexpr
;; encoding: racket jsexpr -> json

;; DECODE
(define (parse-json-bytes bstr)
  (define (parse-fail err) (values #f #f))
  (with-handlers ([exn:fail:read? parse-fail])
    (values (bytes->jsexpr bstr) #t)))

;; (define dog-obj #"{\"dog\": 7}")
;; (define bad-dog-obj #"{\"dog: 7}")
;; (parse-json-bytes bad-dog-obj)

;; ENCODE
(define ht (hash 'apple "red" 'banana "yello"))
(jsexpr->string (hash-set ht 'grapes (list "purple" "green")))

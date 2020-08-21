#lang racket

(require json)
(require web-server/dispatch
         web-server/servlet-env
         web-server/http
         web-server/templates
         txexpr
         (file "../sample-code/respond.rkt"))

(struct product [name sku price] #:transparent)

(define potato (product "potato" 'POT 12.99))
(define carrot (product "carrot" 'CAR 6.49))
(define onion (product "onion" 'ONI 99.95))

(define products (list potato carrot onion))

(define inventory
  (make-hash
   (list
    (cons (product-sku potato) 12)
    (cons (product-sku  carrot) 99)
    (cons (product-sku onion) 1))))

(define (product-exists? sku)
  (hash-has-key? inventory sku)) 

(define (get-prod-by-sku sku)
  (hash-ref inventory sku))

(define (check-inventory sku)
  (hash-ref inventory sku 0))

(define (inventory-enough? sku demand)
  (>= (check-inventory sku) demand))

(define (subtract-from-inventory sku qty)
  (when (and (product-exists? sku) (inventory-enough? sku qty))
    (let ([new-qty (- (check-inventory sku) qty)])
      (hash-set! inventory sku new-qty))))

;; SERVLETS
(define (view-homepage req)
  (let* ([products products]
         [title "Produce Barn"]
         [body (include-template "views/index.html")])
    (respond/html
     #:body (include-template "views/wrapper.html"))))

(define (view-product req name)
  (let* ([product
          (findf
           (lambda (prod) (equal? name (product-name prod)))
           products)]
         [title "Product View"]
         [qty (check-inventory (product-sku product))]
         [body (include-template "views/product.html")])
    (respond/html
     #:body (include-template "views/wrapper.html"))))

(define (add-to-cart req)
  (let* ([post-data (request-post-data/raw req)]
         [data-hash (bytes->jsexpr post-data)]
         [sku (string->symbol (hash-ref data-hash 'sku))]
         [qty (hash-ref data-hash 'qty)])
    (subtract-from-inventory sku qty)
    (respond/json #:code 204)))

(define-values (dispatcher url-generator)
  (dispatch-rules
   [("") #:method "get" view-homepage]
   [((string-arg)) #:method "get" view-product]
   [("add") #:method "post" add-to-cart]))

(serve/servlet
 dispatcher
 #:port 6995
 #:listen-ip #f
 #:launch-browser? #f
 #:servlet-regexp #rx"")


;; respond-json
;; templating script tags and javascript files
;; extracting post data (not form bindings) in servlet
;; defining vars in template scope, then passing them via closure to subtemplate (in this case a js script)

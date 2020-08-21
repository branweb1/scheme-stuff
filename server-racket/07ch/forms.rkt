#lang racket

(require web-server/dispatch
         web-server/servlet-env
         web-server/http
         web-server/templates
         txexpr
         (file "../sample-code/respond.rkt"))

(struct product [name sku price])
(define db (make-hash))
(define current-id 1)

;; string? -> string?
(define (replace-< str)
  (string-replace str
                  "<"
                  "&lt;"))

;; string? -> string?
(define (replace-> str)
  (string-replace str
                  ">"
                  "&gt;"))

;; string? -> string?
(define (html-escape str)
  (replace-< (replace-> str)))

(define (label #:for [for #f] content)
  (txexpr* 'label
           (if (string? for)
               (list (list 'for for))
               empty)
           content))

(define (product-input #:type [type "text"]
                       #:name [name #f]
                       #:required? [required? #f])
  (unless (string? type)
    (error "type should be a string:" type))
  (unless (string? name)
    (error "name should be a string:" name))
  (unless (boolean? required?)
    (error "required? should be a boolean:" required?))
  (txexpr* 'input
           (append (list (list 'type type)
                         (list 'name name))
                   (if required?
                       (list (list 'required "required"))
                       empty))))


(define (product->form prod)
  (define name-input/empty
    (product-input #:name "name" #:required? #t))
  
  (define sku-input/empty
    (product-input #:name "sku" #:required? #t))

  (define price-input/empty
    (product-input #:type "number" #:name "price" #:required? #t))

  (define (enrich-input inp accessor)
    (if (accessor prod)
        (attr-set* inp 'value (html-escape (accessor prod)))
        inp))

  (define name-input/enriched
    (enrich-input name-input/empty product-name))

  (define sku-input/enriched
    (enrich-input sku-input/empty product-sku))

  (define price-input/enriched
    (enrich-input price-input/empty product-price))

  (define form-children
    (list (label #:for "name" "Name")
          name-input/enriched
          (txexpr* 'br)
          (label #:for "sku" "SKU")
          sku-input/enriched
          (txexpr* 'br)
          (label #:for "price" "Price")
          price-input/enriched
          (txexpr* 'input
                   (list (list 'type "submit")))))

  (define price-validations
    (cond [(not (number? (product-price prod))) empty]
          [(> (product-price prod) 0) empty]
          [else
           (list (txexpr* 'p
                          (list (list 'class "error-message"))
                          "Invalid price"))]))

  (txexpr 'form
          (list (list 'action "/catalog")
                (list 'method "post"))
          (append form-children
                  price-validations)))

(define empty-product-form
  (product->form (product #f #f #f)))

;; keyword args so position doesnt matter
;; combined with default args
;; #:keyword [param-name defalt-value]
(define (db-as-html-with-form #:messages [messages empty]
                              #:form [prod-form empty-product-form])
  (let* ([products (hash-values db)]
         [title "Catalog"]
         [body
          (include-template "catalog/index-form-as-var.html")])
    (include-template "wrapper.html")))

(define (get-whole-catalog req)
  (respond #:body (db-as-html-with-form)
           #:mime TEXT/HTML-MIME-TYPE))

(define (form-binding/bytes b)
  (cond ((binding:form? b)
         (binding:form-value b))
        (else
         #f)))

(define (bytes->string b)
  (cond ((bytes? b)
         (with-handlers ([exn:fail:contract? (lambda (e) #f)])
           (bytes->string/utf-8 b)))
        (else
         #f)))

(define (request->product req)
  (let* ([bindings (request-bindings/raw req)]
         [name/binding (bindings-assq #"name" bindings)]
         [sku/binding (bindings-assq #"sku" bindings)]
         [price/binding (bindings-assq #"price" bindings)]
         [name/bytes (form-binding/bytes name/binding)]
         [sku/bytes (form-binding/bytes sku/binding)]
         [price/bytes (form-binding/bytes price/binding)]
         [name/string (bytes->string name/bytes)]
         [sku/string (bytes->string sku/bytes)]
         [price/string (bytes->string price/bytes)]
         [price/number (string->number price/string)])
    (product name/string sku/string price/number)))

(define (product-complete? p)
  (and (string? (product-name p))
       (string? (product-sku p))
       (number? (product-price p))))

(define (create-catalog-item req)
  (define new-prod (request->product req))
  
  (define (respond-with-message status-code message)
    (respond #:code status-code
             #:mime TEXT/HTML-MIME-TYPE
             #:body (db-as-html-with-form
                     #:form (product->form new-prod))))

  (cond [(product-complete? new-prod)
         (hash-set! db current-id new-prod)
         (set! current-id (add1 current-id))
         (respond
          #:code 303
          #:headers (list (cons "Location"
                                (url-generator get-whole-catalog))))]
        [else
         (respond-with-message 400 (product->form new-prod))]))

(define-values (dispatcher url-generator)
  (dispatch-rules
   [("catalog") #:method "get" get-whole-catalog]
   [("catalog") #:method "post" create-catalog-item]))

(serve/servlet
 dispatcher
 #:port 6995
 #:listen-ip #f
 #:launch-browser? #f
 #:servlet-regexp #rx"")

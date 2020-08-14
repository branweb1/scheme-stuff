#lang racket

(struct person [name] #:transparent)
;; struct inheritence
(struct student person [year] #:transparent #:mutable)
(struct professor person [field-of-study] #:transparent)
(struct school [(students #:mutable) professors] #:transparent)

;; (define p1 (person "Jeff"))
;; (define s1 (student "Jeff" "sophomore"))
;; (define prof1 (professor "Bob" "math"))
;; (professor-field-of-study prof1)
;; (person-name prof1)
;; (if (professor? p1) "Professor" "Not a professor")
;; (student? s1)

(define students
  (list (student "vicki" "sophomore")
        (student "john" "senior")
        (student "chad" "senior")
        (student "elvis" "junior")))

(define professors
  (list (professor "mary" "english")
        (professor "doug" "poli-sci")))

(define university (school students professors))

(define (next-year s)
  (case (student-year s)
    [("freshman") "sophomore"]
    [("sophomore") "junior"]
    [("junior") "senior"]
    [("senior") "grad"]))

;; student struct is declared mutable, but students list could not be.
;; can still mutate its elements
(define (roll-year school)
  (for-each
   (lambda (student)
     (set-student-year! student (next-year student)))
   (school-students school)))

;; mutate the students list in school struct instead
(define (roll-year-alt sch)
  (let ([students-next
         (map
          (lambda (st)
            (student (person-name st) (next-year st)))
          (school-students sch))])
    (set-school-students! sch students-next)
;;    (school students-next (school-professors sch))
    ))

;; immutable alternative is to genreate an entirely new school

;; university
;; (roll-year university)
;; university
;;(roll-year-alt university)

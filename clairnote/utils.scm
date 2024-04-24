(define-module (clairnote utils))

(use-modules
  (srfi srfi-1)
  (ice-9 match)
  (lily))

(define (format-loc loc)
  (format #f "~a:~a"
    (assq-ref loc 'filename)
    (assq-ref loc 'line)))

(define (dbg-impl args-q args loc)
  (format (current-error-port)
    "~%~a~%"
    (format-loc loc))

  (map
    (lambda (a b)
      (format (current-error-port) "~a: ~a~%" a b))
    args-q
    args)

  (last args))

(define-syntax-public dbg
  (syntax-rules ()
    ((_ . expr)
      (dbg-impl
        (quote expr)
        (list . expr)
        (current-source-location)))))

(export for-each!)
(define-macro for-each!
  (lambda* (val binding #:rest body)
    `(for-each
      (lambda (x)
       (match-let ((,binding x))
        ,@body))
      ,val)))

;; (define-public (foreach-alist f alist)
;;   (for-each
;;     (lambda (obj)
;;       (f (car obj) (cdr obj)))
;;     alist))

(export push)
(define-macro (push x y)
  `(set! ,y (cons ,x ,y)))

(define-public (close? x y)
  (let ((tolerance 0.01))
    (< (abs (- x y)) tolerance)))

(define-public (context-prop symbol type?)
  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc "custom context property")
  (set! all-translation-properties (cons symbol all-translation-properties))
  symbol)

(define-public (grob-prop symbol type?)
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc "custom grob property")
  symbol)

(define-public (grob-maybe-pure-prop grob sym pure)
  (if pure
    (ly:grob-pure-property grob sym 0 0)
    (ly:grob-property grob sym)))

(define-public (index-cell cell dir)
  (if (equal? dir RIGHT)
    (cdr cell)
    (car cell)))

(define-public (index-set-cell! x dir val)
  (case dir
    ((-1) (set-car! x val))
    ((1) (set-cdr! x val))))

(define-public (grob-extent grob base dir)
  (let* ((common (ly:grob-common-refpoint grob base dir))
         (ext (ly:grob-extent grob common dir))
         (base-coord (ly:grob-relative-coordinate base common dir)))
    (cons (- (car ext) base-coord) (- (cdr ext) base-coord))))

(define-public (grob-coord grob base dir)
  (let* ((common (ly:grob-common-refpoint grob base dir))
         (grob-coord (ly:grob-relative-coordinate grob common dir))
         (base-coord (ly:grob-relative-coordinate base common dir)))
    (- grob-coord base-coord)))

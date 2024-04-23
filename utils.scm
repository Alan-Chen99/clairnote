(define-module (clairnote utils))

(use-modules
  (srfi srfi-1)
  (lily))

(define (format-loc loc)
  (let ((l (ly:input-file-line-char-column loc)))
    (format #f "~a:~a" (first l) (second l))))

(define (dbg-impl args-q args)
  (format (current-error-port)
    "~%~a~%"
    (format-loc (*location*)))

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
        (list . expr)))))

(define-public (context-prop symbol type?)
  (set-object-property! symbol 'translation-type? type?)
  (set-object-property! symbol 'translation-doc "custom context property")
  (set! all-translation-properties (cons symbol all-translation-properties))
  symbol)

(define-public (grob-prop symbol type?)
  (set-object-property! symbol 'backend-type? type?)
  (set-object-property! symbol 'backend-doc "custom grob property")
  symbol)

(define-public (index-cell cell dir)
  (if (equal? dir RIGHT)
    (cdr cell)
    (car cell)))

(define-public (index-set-cell! x dir val)
  (case dir
    ((-1) (set-car! x val))
    ((1) (set-cdr! x val))))

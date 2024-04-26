(define-module (clairnote ledger))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (lily)
  (clairnote utils)
  (clairnote staff)
  (clairnote region)
  (clairnote stencil))

(define-public (LedgerLineSpanner-stencil grob)
  empty-stencil)

(define-public (script-y-offset-callback-unpure grob default-value)
  (match-let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
               (ctx (ly:grob-property staff-symbol 'cn-internal-staff-cached))
               (coord-base (assq-ref ctx #:coord-base))

               (half-staff-space (/ (ly:grob-property staff-symbol 'staff-space 1) 2))

               (direction (ly:grob-property grob 'direction))

               ((left-f . right-f) (grob-extent grob coord-base X))

               ((y1 . y2) (grob-extent grob grob Y))

               (left (ensure-exact-or-inf left-f 1/100))
               (right (ensure-exact-or-inf right-f 1/100))

               (vert-reg (region-box left right -inf.0 +inf.0))

               (avoid-y-range '()))

    (dolist! (reg (assq-ref ctx #:avoid-with-real-ycoord))
      (set! avoid-y-range
        (append
          (region-y-profile (intersect reg vert-reg))
          avoid-y-range)))

    (set! avoid-y-range
      (range-simplify
        (map-list ((a . b) avoid-y-range)
          (cons
            (- (- a (* 1/2 half-staff-space)) y2)
            (- (+ b (* 1/2 half-staff-space)) y1)))))

    (dolist! ((x . y) avoid-y-range)
      (when (< x default-value y)
        (if (< direction 0)
          ;; move at most 1 half-staff-space
          (if (close? default-value x half-staff-space)
            (set! default-value x))
          (if (close? default-value y half-staff-space)
            (set! default-value y)))))

    default-value))

(define-public script-y-offset-callback
  ;; TODO: maybe use grob transformer instead?
  (grob::compose-function
    (ly:make-unpure-pure-container
      script-y-offset-callback-unpure
      (lambda (grob start end v)
        v))
    side-position-interface::y-aligned-side))

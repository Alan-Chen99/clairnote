(define-module (clairnote staff))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (lily)
  (clairnote utils))

;; TODO: make an actual validator
(grob-prop 'cn-staff-style list?)
(grob-prop 'cn-staff-lower integer?)
(grob-prop 'cn-staff-upper integer?)

(grob-prop 'cn-internal-staff-style-normalized list?)
(grob-prop 'cn-internal-staff-style-normalized-pure list?)

(define-public staff-style-default
  `(
    (4
     (#:staff . #t))
    (8
     (#:staff . #t))))

(define-public staff-style-5-lines
  `(
    (0
     ;; (#:color . ,(x11-color 'Gray))
     (#:color . ,(rgb-color 0.5 0.5 0.5))
     (#:staff . #t)
     (#:always-ledger . #t))
    (4
     (#:staff . #t))
    (8
     (#:staff . #t))))

(define (normalize-style-one-pure staff-symbol)
  (lambda (style)
    (let* ((color (assq-ref style #:color))
           (staff (assq-ref style #:staff))
           (ledger (or (not staff) (assq-ref style #:always-ledger))))
      `(
        (#:staff . ,staff)
        (#:color . ,color)
        (#:ledger . ,ledger)))))

(define (normalize-style-one staff-symbol)
  (let* ((thickness (* (ly:grob-property staff-symbol 'thickness 1.0)
                     (ly:output-def-lookup (ly:grob-layout staff-symbol) 'line-thickness)))
         ;; TODO: this is different from Staff_symbol::staff_space
         (staff-space (ly:grob-property staff-symbol 'staff-space 1))
         (tmp-ledger-thickness (ly:grob-property staff-symbol 'ledger-line-thickness '(1.0 . 0.1)))
         (ledger-thickness (+
                            (* (car tmp-ledger-thickness) thickness)
                            (* (cdr tmp-ledger-thickness) staff-space))))
    (lambda (style)
      (let* ((color (assq-ref style #:color))
             (staff (assq-ref style #:staff))
             (ledger (or (not staff) (assq-ref style #:always-ledger))))
        `(
          (#:staff . ,staff)
          (#:color . ,color)
          (#:thickness . ,thickness)
          (#:staff-space . ,staff-space)
          (#:ledger-thickness . ,ledger-thickness)
          (#:ledger . ,ledger))))))

(define ((internal-staff-style-normalized-impl pure) staff-symbol)
  (let ((cn-staff-style (ly:grob-property staff-symbol 'cn-staff-style staff-style-default))
        (norm-f ((if pure normalize-style-one-pure normalize-style-one) staff-symbol))
        (ans '()))
    (for-each! cn-staff-style (h . style)
      (if (number? h)
        ;; always float for consistency
        (push (cons (floor-remainder h 12.0) (norm-f style)) ans)
        (if (eq? h #:default)
          (push (cons #:default (norm-f style)) ans)
          (error "invalid: ~a" h))))

    (unless (assq-ref ans #:default)
      (push (cons #:default (norm-f '())) ans))

    ans))

(define-public internal-staff-style-normalized (internal-staff-style-normalized-impl #f))
(define-public internal-staff-style-normalized-pure (internal-staff-style-normalized-impl #t))

(export style-at-height)
(define* (style-at-height staff-symbol h #:optional pure)
  (let ((staff (ly:grob-property staff-symbol
                (if pure
                  'cn-internal-staff-style-normalized-pure
                  'cn-internal-staff-style-normalized)))
        (ans #f))
    (set! h (floor-remainder h 12.0))
    (for-each! staff (h2 . style)
      (when (and (number? h2) (close? h h2))
        (set! ans style)))
    (or ans (assq-ref staff #:default))))

(define-public style-at-height-one
  (lambda* (staff-symbol h prop #:optional pure)
    (assq-ref (style-at-height staff-symbol h pure) prop)))

(define-public (StaffSymbol-line-positions grob)
  (let ((lower (ly:grob-property grob 'cn-staff-lower))
        (upper (ly:grob-property grob 'cn-staff-upper)))

    (filter
      (lambda (h) (style-at-height-one grob h #:staff #:pure))
      (iota (max 1 (+ (- upper lower) 1)) lower))))

;; TODO: this impl is not complete
(define (staff-line-range staff-symbol)

  ;; (if (and (not (ly:item-break-dir bound))
  ;;      (not (interval-empty? bound-ext)))
  ;;   (index-set-cell! span-points dir
  ;;     (+ (index-cell span-points dir)
  ;;       (index-cell bound-ext dir))))

  (let ((l-b (ly:spanner-bound staff-symbol LEFT))
        (r-b (ly:spanner-bound staff-symbol RIGHT)))
    ;; (dbg (grob-extent r-b staff-symbol X))
    (ly:item-break-dir r-b)

    (cons
      (grob-coord l-b staff-symbol X)
      ;; when extending staff as the same time as changing clef
      ;; we extend more to the right.
      ;; (if (ly:item-break-dir r-b)
      ;;   (grob-coord r-b staff-symbol X)
      ;;   (max
      ;;     (cdr (grob-extent r-b staff-symbol X))
      ;;     (grob-coord r-b staff-symbol X)))
      (max
        (cdr (grob-extent r-b staff-symbol X))
        (grob-coord r-b staff-symbol X)))))

(export make-staff-line-stencil)
(define* (make-staff-line-stencil staff-symbol h startx endx
          #:key
          (color '())
          (thickness #f))
  ;; see lily/staff-symbol.cc Staff_symbol::print
  (let* ((style (style-at-height staff-symbol h))
         (color (if (null? color) (assq-ref style #:color) color))
         (thickness (or thickness (assq-ref style #:thickness)))
         (staff-space (assq-ref style #:staff-space))

         (shrink (/ thickness 2))
         ;; (shrink 0)
         (y (* h staff-space 0.5))
         (bounds (interval-intersection
                  (cons startx endx)
                  (staff-line-range staff-symbol)))
         (ans (make-line-stencil thickness
               (+ (car bounds) shrink)
               y
               (- (cdr bounds) shrink)
               y)))
    (when color
      (if (pair? color)
        (set! ans
          (ly:stencil-in-color ans
            (first color)
            (second color)
            (third color)))
        (set! ans
          (ly:stencil-in-color ans
            color))))
    ans))

(define-public (StaffSymbol-stencil staff-symbol)
  ;; see also
  ;; http://lsr.di.unimi.it/LSR/Item?id=700
  ;; (by Neil Puttock)

  ;; (dbg 'StaffSymbol-stencil)
  ;; (dbg
  ;;   staff-symbol
  ;;   (ly:grob-property staff-symbol 'width)

  ;;   (ly:spanner-bound staff-symbol LEFT)
  ;;   (ly:spanner-bound staff-symbol RIGHT)

  ;;   (ly:grob-object (ly:spanner-bound staff-symbol LEFT) 'elements)
  ;;   (ly:grob-object (ly:spanner-bound staff-symbol RIGHT) 'elements)

  ;;   (grob-coord (ly:spanner-bound staff-symbol LEFT) staff-symbol X)
  ;;   (grob-coord (ly:spanner-bound staff-symbol RIGHT) staff-symbol X)
  ;;   (grob-extent (ly:spanner-bound staff-symbol LEFT) staff-symbol X)
  ;;   (grob-extent (ly:spanner-bound staff-symbol RIGHT) staff-symbol X)

  ;;   (ly:item-break-dir (ly:spanner-bound staff-symbol LEFT))
  ;;   (ly:item-break-dir (ly:spanner-bound staff-symbol RIGHT)))

  (let ((ans empty-stencil))
    (for-each
      (lambda (h)
        (set! ans
          (ly:stencil-add
            ans
            (make-staff-line-stencil staff-symbol h -inf.0 +inf.0))))
      (ly:grob-property staff-symbol 'line-positions))
    ans))

(define-public (extend-staff context reset going-up going-down)
  (let ((grob-def (ly:context-grob-definition context 'StaffSymbol)))

    (ly:context-pushpop-property context 'StaffSymbol 'cn-staff-upper
      (+ (if reset 8 (ly:assoc-get 'cn-staff-upper grob-def)) (* 12 going-up)))

    (ly:context-pushpop-property context 'StaffSymbol 'cn-staff-lower
      (- (if reset -8 (ly:assoc-get 'cn-staff-lower grob-def)) (* 12 going-down)))))

(define-module (clairnote staff))

(use-modules
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (lily)
  (clairnote utils))

;; (define (staff-line-stan grob)
;;   (let* ((common (ly:grob-system grob))
;;          (span-points '(0 . 0))
;;          (thickness (* (ly:grob-property grob 'thickness 1.0)
;;                      (ly:output-def-lookup (ly:grob-layout grob) 'line-thickness)))
;;          (width (ly:grob-property grob 'width)))

;;     (for-each
;;       (lambda (dir)
;;         (if (and (= dir RIGHT)
;;              (number? width))
;;           (set-cdr! span-points width)
;;           (let* ((bound (ly:spanner-bound grob dir))
;;                  (bound-ext (ly:grob-extent bound bound X)))

;;             (index-set-cell! span-points dir
;;               (ly:grob-relative-coordinate bound common X))
;;             (if (and (not (ly:item-break-dir bound))
;;                  (not (interval-empty? bound-ext)))
;;               (index-set-cell! span-points dir
;;                 (+ (index-cell span-points dir)
;;                   (index-cell bound-ext dir))))))
;;         (index-set-cell! span-points dir (- (index-cell span-points dir)
;;                                           (* dir thickness 0.5))))
;;       (list LEFT RIGHT))

;;     (set! span-points
;;       (coord-translate span-points
;;         (- (ly:grob-relative-coordinate grob common X))))

;;     (make-line-stencil thickness (car span-points) 0 (cdr span-points) 0)))

;; http://lsr.di.unimi.it/LSR/Item?id=700
;; %LSR This snippet was contributed by Neil Puttock
(define-public ((color-staff-lines . rest) grob)

  (define (index-cell cell dir)
    (if (equal? dir RIGHT)
      (cdr cell)
      (car cell)))

  (define (index-set-cell! x dir val)
    (case dir
      ((-1) (set-car! x val))
      ((1) (set-cdr! x val))))

  (let* ((common (ly:grob-system grob))
         (span-points '(0 . 0))
         (thickness (* (ly:grob-property grob 'thickness 1.0)
                     (ly:output-def-lookup (ly:grob-layout grob) 'line-thickness)))
         (width (ly:grob-property grob 'width))
         (line-positions (ly:grob-property grob 'line-positions))
         (staff-space (ly:grob-property grob 'staff-space 1))
         (line-stencil #f)
         (total-lines empty-stencil)
         ;; use a local copy of colors list, since
         ;; stencil creation mutates list
         (colors rest))

    (for-each
      (lambda (dir)
        (if (and (= dir RIGHT)
             (number? width))
          (set-cdr! span-points width)
          (let* ((bound (ly:spanner-bound grob dir))
                 (bound-ext (ly:grob-extent bound bound X)))

            (index-set-cell! span-points dir
              (ly:grob-relative-coordinate bound common X))
            (if (and (not (ly:item-break-dir bound))
                 (not (interval-empty? bound-ext)))
              (index-set-cell! span-points dir
                (+ (index-cell span-points dir)
                  (index-cell bound-ext dir))))))
        (index-set-cell! span-points dir (- (index-cell span-points dir)
                                          (* dir thickness 0.5))))
      (list LEFT RIGHT))

    (set! span-points
      (coord-translate span-points
        (- (ly:grob-relative-coordinate grob common X))))
    (set! line-stencil
      (make-line-stencil thickness (car span-points) 0 (cdr span-points) 0))

    (if (pair? line-positions)
      (for-each (lambda (position)
                 (let ((color (if (pair? colors)
                               (car colors)
                               #f)))
                   (set! total-lines
                     (ly:stencil-add
                       total-lines
                       (ly:stencil-translate-axis
                         (if (color? color)
                           (ly:stencil-in-color line-stencil
                             (first color)
                             (second color)
                             (third color))
                           line-stencil)
                         (* position staff-space 0.5)
                         Y)))
                   (and (pair? colors)
                     (set! colors (cdr colors)))))
        line-positions)
      (let* ((line-count (ly:grob-property grob 'line-count 5))
             (height (* (1- line-count) (/ staff-space 2))))
        (do ((i 0 (1+ i)))
          ((= i line-count))
          (let ((color (if (and (pair? colors)
                            (> (length colors) i))
                        (list-ref colors i)
                        #f)))
            (set! total-lines (ly:stencil-add
                               total-lines
                               (ly:stencil-translate-axis
                                 (if (color? color)
                                   (ly:stencil-in-color line-stencil
                                     (first color)
                                     (second color)
                                     (third color))
                                   line-stencil)
                                 (- height (* i staff-space))
                                 Y)))))))
    total-lines))

(define (normalize-staff-spec-1 spec)
  (if (integer? spec)
    (set! spec `(,spec (#:staff . #t)))
    (set! spec (list-copy spec)))
  spec)

(define (normalize-staff-spec spec)
  (map normalize-staff-spec-1 spec))

(define-public default-staff
  `(
    -8
    -4
    (0
     (#:color . ,(x11-color 'Gray))
     (#:ignore-for-ledger . #t))
    4
    8))

(define-public (staff-style-at-height staff-symbol h)
  (let ((style '()))
    (for-each
      (lambda (x)
        (when (eqv? (modulo (- (car x) h) 12) 0)
          (set! style x)))
      (normalize-staff-spec (ly:grob-property staff-symbol 'cn-staff-base '(-8 -4 4 8))))
    (let* ((color (assq-ref style #:color))
           (staff (assq-ref style #:staff))
           (ledger (or (not staff) (assq-ref style #:ignore-for-ledger)))
           (thickness (* (ly:grob-property staff-symbol 'thickness 1.0)
                       (ly:output-def-lookup (ly:grob-layout staff-symbol) 'line-thickness)))
           ;; TODO: this is different from Staff_symbol::staff_space
           (staff-space (ly:grob-property staff-symbol 'staff-space 1))
           (tmp-ledger-thickness (ly:grob-property staff-symbol 'ledger-line-thickness '(1.0 . 0.1)))
           (ledger-thickness (+
                              (* (car tmp-ledger-thickness) thickness)
                              (* (cdr tmp-ledger-thickness) staff-space))))
      `(
        (#:color . ,color)
        (#:thickness . ,thickness)
        (#:staff-space . ,staff-space)
        (#:ledger-thickness . ,ledger-thickness)
        (#:ledger . ,ledger)))))

(define-public (make-staff-line-stencil staff-symbol h)
  (let* ((style (staff-style-at-height staff-symbol h))
         (color (assq-ref style #:color))
         (thickness (assq-ref style #:thickness)))
    (lambda (startx starty endx endy)
      (let ((ans (make-line-stencil thickness startx starty endx endy)))
        (when color
          (unless (color? color)
            (error (format #f "invalid color: ~a" color)))
          (set! ans
            (ly:stencil-in-color ans
              (first color)
              (second color)
              (third color))))
        ans))))

(define (compute-staff base ext-down ext-up)
  (dbg 'compute-staff ext-down ext-up)
  (let ((base-normalized (normalize-staff-spec base))
        (ans '()))

    (do ((i ext-down (1+ i)))
      ((> i ext-up))

      (for-each
        (lambda (x)
          (set! ans
            (assoc-set! ans (+ (* i 12) (car x)) (cdr x))))
        base-normalized))

    (set! ans (sort! ans (lambda (a b) (< (car a) (car b)))))
    ans))

(grob-prop 'cn-staff list?)
(grob-prop 'cn-staff-to-draw list?)
(grob-prop 'cn-staff-base list?)
(grob-prop 'cn-staff-ext-down integer?)
(grob-prop 'cn-staff-ext-up integer?)
(grob-prop 'cn-line-positions-for-ledger list?)

(define-public (StaffSymbol-cn-staff grob)
  (compute-staff
    (ly:grob-property grob 'cn-staff-base '(-8 -4 4 8))
    (ly:grob-property grob 'cn-staff-ext-down 0)
    (ly:grob-property grob 'cn-staff-ext-up 0)))

(define-public (StaffSymbol-line-positions grob)
  (map car (ly:grob-property grob 'cn-staff)))

(define (round-down12 x)
  (/ (- x (modulo x 12)) 12))

(define (round-up12 x)
  (- (round-down12 (- x))))

(define-public (StaffSymbol-cn-staff-to-draw grob)
  ;; (let* ((grobs (ly:grob-array->list (ly:grob-object (ly:grob-system grob) 'all-elements)))
  ;;        (pos (filter-map
  ;;              (lambda (x)
  ;;                (if (and
  ;;                     (eq? (grob::name x) 'NoteHead)
  ;;                     (eq? (ly:grob-object x 'staff-symbol) grob))
  ;;                  (ly:grob-property x 'staff-position)
  ;;                  #f))
  ;;              grobs))
  ;;        (pos-min (if (null? pos) 0 (apply min pos)))
  ;;        (pos-max (max (+ pos-min 1) (if (null? pos) 0 (apply max pos))))
  ;;        (min-reach -12)
  ;;        (max-reach 12)
  ;;        (ext-down (round-down12 (- pos-min min-reach)))
  ;;        (ext-up (round-up12 (- pos-max max-reach))))

  ;;   (compute-staff
  ;;     (ly:grob-property grob 'cn-staff-base '(-8 -4 4 8))
  ;;     (min ext-down (ly:grob-property grob 'cn-staff-ext-down 0))
  ;;     (max ext-up (ly:grob-property grob 'cn-staff-ext-up 0))))
  (ly:grob-property grob 'cn-staff))

(define-public (StaffSymbol-stencil grob)
  ((apply color-staff-lines
      (map
        (lambda (x) (assoc-ref (cdr x) #:color))
        (ly:grob-property grob 'cn-staff)))
    grob))

;; (define-public StaffSymbol-Y-extent
;;   ;; grob::unpure-Y-extent-from-stencil
;;   (ly:make-unpure-pure-container
;;     (lambda (grob . rest)
;;       (dbg 'StaffSymbol-Y-extent-unpure
;;         rest
;;         grob
;;         (ly:grob-system grob)
;;         (ly:grob-property grob 'line-positions))
;;       (/ 1 0)
;;       '(-4 . 4))))

;; (define (StaffSymbol-cn-line-positions-for-ledger-unpure grob . _)
;;   (map car
;;     (filter
;;       (lambda (x) (not (assoc-ref (cdr x) #:ignore-for-ledger)))
;;       (ly:grob-property grob 'cn-staff))))

;; (define-public StaffSymbol-cn-line-positions-for-ledger
;;   (ly:make-unpure-pure-container StaffSymbol-cn-line-positions-for-ledger-unpure))

(define-public (extend-staff context reset going-up going-down)
  (let ((grob-def (ly:context-grob-definition context 'StaffSymbol)))

    ;; (dbg grob-def)

    (ly:context-pushpop-property context 'StaffSymbol 'cn-staff-ext-up
      (+ (ly:assoc-get 'cn-staff-ext-up grob-def) going-up))

    (ly:context-pushpop-property context 'StaffSymbol 'cn-staff-ext-down
      (- (ly:assoc-get 'cn-staff-ext-down grob-def) going-down))))

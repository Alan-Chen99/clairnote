(define-module (clairnote ledger))

(use-modules
  (ice-9 match)
  (ice-9 curried-definitions)
  (srfi srfi-1)
  (lily)
  (clairnote utils)
  (clairnote staff))

(define (cn-ledger-pattern dist staff-symbol)
  ;; Produces the ledger line pattern for a given note.
  ;; dist is distance of note from closest staff line.

  ;; A cycle is generally an octave, so cycle-size is
  ;; 12 (staff positions per octave) and cycle-count
  ;; is the number of octaves we are dealing with.
  (let* ((recipe (ly:grob-property staff-symbol 'cn-ledger-recipe #|; cn-dn-ledgers-gradual|#))
         (cycle-size (list-ref recipe 0))
         (configs (list-ref recipe 1))
         (cycle-count (+ 1 (quotient dist cycle-size)))
         (config-to-ledgers
           (lambda (config)
             (let*
               ((ledger-posn (list-ref config 0))
                 (further-add (list-ref config 1))
                 (nearer-drop (list-ref config 2))
                 (furthest (+ dist further-add))
                 (nearest (if nearer-drop (- dist nearer-drop) 0))
                 ;; Generate the list of ledgers in descending order.
                 (ledgers (reverse (iota cycle-count ledger-posn cycle-size)))
                 (in-range (lambda (l) (and (<= l furthest)
                                        (>= l nearest)))))
               (filter in-range ledgers))))
         (ledger-lists (map config-to-ledgers configs))

         ;; When there is an accidental sign on a note, LilyPond may
         ;; shorten the furthest ledger, the first in the list.
         ;; So we merge into one list, keeping descending order
         (ledger-list (fold (lambda (lst result) (merge lst result >))
                       '()
                       ledger-lists))
         ;; But if the note is on a ledger, move that ledger to the
         ;; front of the list, so that ledger will be shortened.
         (ledger-list2 (if (memv dist ledger-list)
                        (cons dist (delv dist ledger-list))
                        ledger-list)))
    ledger-list2))

(define-public (cn-ledger-positions staff-symbol pos)
  ;; A function that takes a StaffSymbol grob and a vertical
  ;; position of a note head and returns a list of ledger line positions,
  ;; based on StaffSymbol.cn-ledger-recipe."
  (let*
    ((staff-lines (filter
                   (lambda (h)
                     (not (assq-ref (style-at-height staff-symbol h #:pure) #:ledger)))
                   (ly:grob-property staff-symbol 'line-positions)))
      (lines (if (null? staff-lines) '(0) staff-lines))
      (nearest-line
        (fold (lambda (line prev)
               (if (< (abs (- line pos)) (abs (- prev pos)))
                 line
                 prev))
          (car lines)
          (cdr lines)))

      (diff (- pos nearest-line))
      (dist (abs diff))
      (dir (if (negative? diff) -1 1))

      ;; get generic ledger positions and then transform them so
      ;; they are relative to nearest-line and in the right direction
      (ledgers0 (cn-ledger-pattern dist staff-symbol))
      (ledgers1 (map (lambda (n) (+ nearest-line (* dir n)))
                 ledgers0))

      ;; remove any ledgers that would fall on staff lines
      (ledgers2 (filter (lambda (n) (not (member n lines)))
                 ledgers1)))
    ledgers2))

(define (fuse-intervals vals)
  (let ((cur-start #f)
        (cur-end #f)
        (ans '()))

    (for-each
      (lambda (x)
        (if (and cur-start (<= (car x) cur-end))
          (set! cur-end (cdr x))

          (begin
            (when cur-start
              (set! ans (cons (cons cur-start cur-end) ans)))
            (set! cur-start (car x))
            (set! cur-end (cdr x))))
        x)
      vals)

    (set! ans (cons (cons cur-start cur-end) ans))
    (reverse ans)))

(define (remove-small-gaps vals)
  (let ((changed #t))
    (while changed
      (set! changed #f)
      (let ((cur-start #f)
            (cur-end #f)
            (ans '()))
        (for-each
          (lambda (x)
            (if (and cur-start (< (* 5 (- (car x) cur-end)) (- (cdr x) cur-start)))
              (begin
                (set! changed #t)
                (set! cur-end (cdr x)))
              (begin
                (when cur-start
                  (set! ans (cons (cons cur-start cur-end) ans)))
                (set! cur-start (car x))
                (set! cur-end (cdr x))))
            x)
          vals)

        (set! ans (cons (cons cur-start cur-end) ans))
        (set! vals (reverse ans))))
    vals))

(define-public (LedgerLineSpanner-stencil grob)
  (let* ((staff-symbol (ly:grob-object grob 'staff-symbol)))
    ;; regression/staff-online-symbol-absence.ly
    (if (null? staff-symbol)
      '()
      (LedgerLineSpanner-stencil-impl staff-symbol grob))))

(define (LedgerLineSpanner-stencil-impl staff-symbol grob)
  ;; lily/ledger-line-spanner.cc
  (let* ((line-positions (ly:grob-property staff-symbol 'line-positions))
         (note-heads-arr (ly:grob-object grob 'note-heads))
         (note-heads (if (null? note-heads-arr) '()
                      (ly:grob-array->list note-heads-arr)))
         (length_fraction (ly:grob-property grob 'length-fraction 0.25))
         (staff-space (ly:grob-property staff-symbol 'staff-space 1))
         (pos-f (eval
                 (ly:grob-property staff-symbol 'ledger-positions-function)
                 (interaction-environment)))

         (grob-x #f)
         (ledgers-alist '())
         (res-stencil empty-stencil))

    (for-each! note-heads nh
      (let* ((x-extent (grob-extent nh grob X))
             (y-p (ly:grob-property nh 'staff-position))
             (y-pos '())
             (tmp (ly:grob-property nh 'ledger-positions)))
        (if (pair? tmp)
          (set! y-pos tmp)
          (set! y-pos (pos-f staff-symbol y-p)))

        (for-each! y-pos p
          (set! ledgers-alist
            (assv-set! ledgers-alist p
              (cons x-extent (or (assv-ref ledgers-alist p) '())))))))

    (for-each! ledgers-alist (h . intervals-list)
      (let* ((style (style-at-height staff-symbol h))
             (ledger-thickness (assq-ref style #:ledger-thickness))
             ;; (y-p (+ base-coord-y (* (car ele) staff-space 0.5)))
             (line (sort! intervals-list (lambda (a b) (< (car a) (car b))))))

        (when (and (assq-ref style #:staff) (not (memv h line-positions)))
          (let ((intervals (remove-small-gaps
                            (fuse-intervals
                              (map (lambda (x)
                                    (interval-widen x
                                      (* (interval-length x) 2)))
                                line)))))
            (for-each! intervals (lb . ub)
              (set! res-stencil
                (ly:stencil-add
                  res-stencil
                  (make-staff-line-stencil staff-symbol h lb ub))))))

        (when (assq-ref style #:ledger)
          (let ((intervals (fuse-intervals
                            (map (lambda (x)
                                  (interval-widen x
                                    (* (interval-length x) length_fraction)))
                              line))))
            (for-each! intervals (lb . ub)
              (set! res-stencil
                (ly:stencil-add res-stencil
                  (make-staff-line-stencil staff-symbol h lb ub #:thickness ledger-thickness #:color #f))))))))

    res-stencil))

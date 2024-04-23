(define-module (clairnote ledger))

(use-modules
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
    ;; (dbg
    ;;   dist
    ;;   recipe
    ;;   ledger-list2)
    ledger-list2))

(define-public (cn-ledger-positions staff-symbol pos)
  ;; A function that takes a StaffSymbol grob and a vertical
  ;; position of a note head and returns a list of ledger line positions,
  ;; based on StaffSymbol.cn-ledger-recipe."
  (let*
    ((lines (map car
             (filter
               (lambda (x) (not (assoc-ref (cdr x) #:ignore-for-ledger)))
               (ly:grob-property staff-symbol 'cn-staff))))

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

;; lily/ledger-line-spanner.cc
(define-public (LedgerLineSpanner-stencil grob)

  (let* ((staff-symbol (ly:grob-object grob 'staff-symbol))
         (note-heads (ly:grob-array->list (ly:grob-object grob 'note-heads)))
         (length_fraction (ly:grob-property grob 'length-fraction 0.25))
         (staff-space (ly:grob-property staff-symbol 'staff-space 1))
         (pos-f (eval
                 (ly:grob-property staff-symbol 'ledger-positions-function)
                 (interaction-environment)))
         (common-y (ly:grob-common-refpoint grob staff-symbol Y))
         (base-coord-y
           (-
             (ly:grob-relative-coordinate staff-symbol common-y Y)
             (ly:grob-relative-coordinate grob common-y Y)))

         (common-x grob)
         (grob-x #f)
         (ledgers-alist '())
         (res-stencil empty-stencil))
    (for-each
      (lambda (h)
        (set! common-x (ly:grob-common-refpoint common-x h X))
        (when (ly:grob? (ly:grob-object h 'accidental-grob))
          (set! common-x (ly:grob-common-refpoint common-x (ly:grob-object h 'accidental-grob) X))))
      note-heads)
    (set! grob-x (ly:grob-relative-coordinate grob common-x X))

    (for-each
      (lambda (h)
        (let* ((x-c (coord-translate (ly:grob-extent h common-x X) (- grob-x)))
               (y-p (ly:grob-property h 'staff-position))
               (y-pos #f)
               (tmp #f))
          (set! tmp (ly:grob-property h 'ledger-positions))
          (if (pair? tmp)
            (set! y-pos tmp)
            (set! y-pos (pos-f staff-symbol y-p)))

          (for-each
            (lambda (p)
              (set! ledgers-alist
                (assv-set! ledgers-alist p
                  (cons x-c (or (assv-ref ledgers-alist p) '())))))
            y-pos)))

      note-heads)

    (for-each
      (lambda (ele)
        (let* ((style (staff-style-at-height staff-symbol (car ele)))
               (ledger-thickness (assq-ref style #:ledger-thickness))
               (y-p (+ base-coord-y (* (car ele) staff-space 0.5)))
               (line (sort! (cdr ele) (lambda (a b) (< (car a) (car b)))))
               (stencil-f (make-staff-line-stencil staff-symbol (car ele))))

          (for-each
            (lambda (x)
              (set! res-stencil
                (ly:stencil-add
                  res-stencil
                  (stencil-f (car x) y-p (cdr x) y-p))))
            (remove-small-gaps
              (fuse-intervals
                (map (lambda (x)
                      (interval-widen x
                        (* (interval-length x) 2)))
                  line))))

          (when (assq-ref style #:ledger)
            (for-each
              (lambda (x)
                (set! res-stencil
                  (ly:stencil-add res-stencil
                    (make-line-stencil ledger-thickness (car x) y-p (cdr x) y-p))))
              (fuse-intervals
                (map (lambda (x)
                      (interval-widen x
                        (* (interval-length x) length_fraction)))
                  line))))))
      ledgers-alist)

    res-stencil))

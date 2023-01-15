;; Graph related functions from chapter 15 of Emacs Lisp Intro
(defvar graph-symbol "*"
  "String used as symbol in graph, usually an asterisk.")

(defvar graph-blank " "
  "String used as blank in graph, usually a blank space.
graph-blank must be the same number of columns wide
as graph-symbol.")

(defvar graph-max-height 20
  "The maximum height a graph column should be.")

(defun graph-body-print (numbers-list)
  "Insert a body of a graph at point, whose column values are the contents of `NUMBERS-LIST'.

The max value of a column should be scaled to is defined as `graph-max-height'.  The largest value
of `NUMBERS-LIST' will be translated to that value, and all other values will be translated by the
same amount to avoid overly large columns from displaying."
  (let ((max-height (apply 'max numbers-list)))
    (while numbers-list
    ;; Draw column.
      (insert-rectangle
       (column-of-graph graph-max-height (translate-height max-height (car numbers-list))))

    ;; Reposition point.
      (forward-line (- graph-max-height))
      (move-end-of-line nil)

      ;; Shrink numbers-list.
      (setq numbers-list (cdr numbers-list)))))

(defun column-of-graph (max-graph-height actual-height)
  "Returns a list of strings representation of a column of a graph of length `MAX-GRAPH-HEIGHT'.

`ACTUAL-HEIGHT' is the number of graph-symbols that will appear in the column. Graph symbols are represented as the value
of the variable `graph-symbol', and the empty filler spaces are represented as the value of `graph-blank'."
  (let ((insert-list nil)
	(number-of-top-blanks (- max-graph-height actual-height)))
    ;; Fill in asterisks.
    (while (> actual-height 0)
      (setq insert-list (cons graph-symbol insert-list))
      (setq actual-height (1- actual-height)))

    ;; Fill in blanks.
    (while (> number-of-top-blanks 0)
      (setq insert-list (cons graph-blank insert-list))
      (setq number-of-top-blanks (1- number-of-top-blanks)))

    ;; Return whole list.
    insert-list))

(defun translate-height (max-height actual-height)
  "Given an untranslated height of `ACTUAL-HEIGHT', scale it down to fit within the bounds of `graph-max-height'."
  ;; If unset, default graph-max-height to 20.
  (unless graph-max-height
    (setq graph-max-height 20))
  (truncate (* (/ (float graph-max-height) max-height) actual-height)))

(provide 'graph)

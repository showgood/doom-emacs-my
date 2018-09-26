;;; org/org/autoload/tables.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/table-next-row ()
  "Go to the next row (same column) in the current table."
  (interactive)
  (if (org-at-table-p)
      (org-table-next-row)
    (org-down-element)))

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (if (org-at-table-p)
      (progn
        (org-table-maybe-eval-formula)
        (org-table-maybe-recalculate-line)
        (if (and org-table-automatic-realign
                 org-table-may-need-update)
            (org-table-align))
        (let ((col (org-table-current-column)))
          (beginning-of-line 0)
          (when (or (not (org-at-table-p)) (org-at-table-hline-p))
            (beginning-of-line))
          (org-table-goto-column col)
          (skip-chars-backward "^|\n\r")
          (when (org-looking-at-p " ") (forward-char))))
    (org-up-element)))

;;;###autoload
(defun +org/table-next-field ()
  (interactive)
  (if (org-at-table-p) (org-table-next-field) (org-end-of-line)))

;;;###autoload
(defun +org/table-previous-field ()
  (interactive)
  (if (org-at-table-p) (org-table-previous-field) (org-beginning-of-line)))

;;;###autoload
(defun +org/table-append-field-or-shift-right ()
  (interactive)
  (org-shiftmetaright)
  (when (org-at-table-p) (org-metaright)))

;;;###autoload
(defun +org/table-prepend-field-or-shift-left ()
  (interactive)
  (if (org-at-table-p) (org-shiftmetaright) (org-shiftmetaleft)))

;;;###autoload
(defun me/org-append-formula (cell formula)
  (interactive)
  (let ((formulas (org-table-get-stored-formulas)))
    (setq formulas (cons (cons cell formula) formulas))
        (org-table-store-formulas formulas)
        (org-table-iterate)
    )
)

;;;###autoload
(defun me/org-sum-rows (begin-row)
  "insert a org-table formula for sum all the rows for current column
    at the cell under cursor"
  (interactive "p")
  (cond ((or (<= begin-row 0)
             (>= begin-row (org-table-current-dline)) )
         (error "invalid row!"))
        (t
         (let ((cell (format "@%d$%d" (org-table-current-dline) (org-table-current-column))))
           (if (= begin-row 1)
               (me/org-append-formula cell
                                      (format "vsum(@2..@%d)" (- (org-table-current-dline) 1)))
             (me/org-append-formula cell
                                    (format "vsum(@%d..@%d)" begin-row (- (org-table-current-dline) 1)))
             )
           )
         )
        ))

;;;###autoload
(defun me/org-sum-cols (begin-column)
  "insert a org-table formula for sum columns begin-column -- current-column -1
   for current row at the cell under cursor"
  (interactive "p")
  (cond ((or (<= begin-column 0)
             (>= begin-column (org-table-current-column)) )
         (error "invalid column!"))
        (t
         (let (
               (cell (format "@%d$%d" (org-table-current-dline) (org-table-current-column)) )
               (sum-formula (format "vsum($%d..$%d)" begin-column (- (org-table-current-column) 1) )))
           (me/org-append-formula cell sum-formula)
           )
         )
        )
  )

;;;###autoload
(defun me/org-add-index-row ()
  (interactive)
  (org-table-goto-column 1)
  (org-table-insert-column)
  (me/org-append-formula "$1" "@#-1")
)

;;;###autoload
(defun me/create-org-table-from-clipboard ()
  "create a table in org mode with content from clipboard"
  (interactive)
  (let* ((buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (insert (get-kill-ring))
      (mark-whole-buffer)
      (org-table-create-or-convert-from-region nil)
      (org-table-insert-hline)
      (goto-char (point-min))
      (open-line 1)
      (insert "#+tblname:")
      (append-to-buffer buf (point-min) (point-max))
      )))

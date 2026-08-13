;;; $DOOMDIR/autoload/self-editing.el -*- lexical-binding: t; -*-

;; commands for manipulating text

(defconst self/date-format-options '(("MM/YYYY"    . "%m/%Y")
                                     ("MM/DD"      . "%m/%d")
                                     ("MM/DD/YYYY" . "%m/%d/%Y")
                                     ("YYYYMMDD" . "%Y%m%d")
                                     ("YYYY-MM-DD" . "%Y-%m-%d"))
  "Various date formats used in interactive functions.")

;; thank you xah
;;;###autoload
(defun self/unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;;###autoload
(defun self/calendar-insert-date ()
  "Using `org-read-date', insert the returned date."
  (interactive)
  (let* ((date (org-read-date nil t))
         (option (completing-read "Select a format: " (mapcar 'car self/date-format-options)))
         (output (format-time-string (cdr (assoc option self/date-format-options)) date)))
    (insert output)))

;;;###autoload
(defun self/insert-current-date-at-point ()
  "Inserts date at point in the chosen format."
  (interactive)
  (let* ((output (self/format-date-from-option (self/choose-date-format))))
    (with-current-buffer (current-buffer)
      (insert output))))

;;;###autoload
(defun self/copy-line-number-reference (arg)
  (interactive "p")
  (when-let* ((file-name (buffer-file-name)))
    (pcase arg
      (1 (kill-new (format "%s:%s" file-name (line-number-at-pos))))
      (4 (kill-new (format "%s:%s:%s" file-name (line-number-at-pos) (current-column)))))))

;; thank you EmacsWiki
;;;###autoload
(defun self/sort-words-in-region (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

;;;###autoload
(defun self/suspend-save ()
  "A stupid hack to allow for things like saving without formatting."
  (interactive)
  (major-mode-suspend)
  (text-mode)
  (save-buffer)
  (major-mode-restore))

;;;###autoload
(defun self/fill-line-length-with-character (char &optional direction)
  "Inserts a line of CHAR of current line length above and below the current
line. One prefix argument only adds the bottom line, and two prefix arguments
only adds the top line."
  (interactive "sChar: \np")
  (when (> (length char) 1)
    (user-error "This function only supports filling lines with one character at the moment!"))
  (when (stringp char)
    (setq char (string-to-char char)))
  (let* ((line-length (- (line-end-position) (line-beginning-position)))
         (new-text (make-string line-length char)))
    (save-excursion
      (pcase direction
        ;; TODO should probably check to see if those lines are empty!
        (4 (progn
             (forward-line 1)
             (insert new-text)))
        (16 (progn
              (forward-line -1)
              (insert new-text)))
        (_ (progn
             (forward-line 1)
             (insert new-text)
             (forward-line -2)
             (insert new-text)))))))

;; thank you ChatGPT
;;;###autoload
(defun self/create-centered-comment (text)
  "Create a vertically centered comment with the given TEXT."
  (interactive "sEnter comment: ")
  (let* ((comment-start (concat comment-start " "))
         (comment-end (concat " " comment-end))
         (available-width (- fill-column (length comment-start) (length comment-end) 2))
         (padding-width (/ (- available-width (length text)) 2))
         (padding (make-string padding-width ?-))
         (centered-text (concat padding " " text " " padding)))
    (save-excursion
      (insert (concat comment-start centered-text comment-end)))))

;;;###autoload
(defun self/surround-line-with-character (char)
  "Surrounds the current line with a character CHAR.
For example, give this line of text:

hello world

returns:

===========
hello world
==========="
  (interactive "sChar: ")
  (let ((fill (make-string (- (line-end-position) (line-beginning-position))
                           (string-to-char char))))
    (save-excursion
      (beginning-of-line)
      (insert fill "\n"))
    (save-excursion
      (end-of-line)
      (insert "\n" fill))))

;; thank you ChatGPT
;;;###autoload
(defun self/evil-ex-shuffle-lines (beg end)
  "Shuffle the lines in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((lines (split-string (buffer-substring (point-min) (point-max)) "\n" t)))
      (self/shuffle lines)
      (delete-region beg end)
      (insert (mapconcat #'identity lines "\n"))))
  (widen))

;; thank you ChatGPT
;;;###autoload
(defun self/evil-ex-remove-duplicates (beg end)
  "Remove duplicate lines in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (narrow-to-region beg end)
    (delete-duplicate-lines (point-min) (point-max))
    (widen)))

(defun self/choose-date-format ()
  "Provides user with options from `self/date-format-options'."
  (completing-read "Select a format: " (mapcar 'car self/date-format-options)))

(defun self/format-date-from-option (option)
  "Formats current date according to selected date option."
  (format-time-string (cdr (assoc option self/date-format-options))))

(defun self/shuffle (lst)
  "Shuffles a list LST."
  (let ((n (length lst)))
    (dotimes (i (length lst))
      (let ((j (+ i (random (- n i)))))
        (when (/= i j)
          (cl-rotatef (elt lst i) (elt lst j))))))
  lst)

;;; ../Sources/dotfiles/doom.d/scripts/work.el -*- lexical-binding: t; -*-

;; defines work-related functions, i.e. stuff related to getting my day job done

(require 'request)

(defconst work/tasks-pattern (rx "Tasks" (zero-or-more any)))

(defvar work/email nil "Work email. Defaults to nil for privacy reasons.")

(defun work/get-lorem-ipsum (arg)
  (interactive "P")
  (let ((paragraph-count arg)
        (ret nil))
    (when (eq paragraph-count nil)
      (setq paragraph-count (string-to-number (read-from-minibuffer "How many paragraphs? " "5"))))

    (request "https://lipsum.com/feed/json"
      :type "POST"
      :data `(("amount" . ,paragraph-count))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq ret (cdr (assoc 'lipsum (assoc 'feed data)))))))
    (kill-new ret)
    (message "Lorem Ipsum saved to kill ring")))

(defun work/insert-task-header ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert (format "** Tasks [/]
:PROPERTIES:
:LOGGING:  %s
:END:" work/logging-properties))))

;; non-interactive

(defun work/get-journal-headings (&rest headings)
  (with-temp-buffer
    (org-journal-previous-entry)
    (seq-filter
     (lambda (val)
       (not (eq val nil)))
     (org-map-entries
      (lambda ()
        (let* ((heading-comp (org-heading-components))
               (title (nth 4 heading-comp))
               (level (car heading-comp)))
          (when
              (or
               (eq level 2)
               (member title headings)
               (string-match-p work/tasks-pattern title))
            (if (string-match-p work/tasks-pattern title)
                "Tasks"
              title))))))))

(defun work/org-element-filter (seq types pred)
  (let ((col nil))
    (org-element-map seq types (lambda (el)
                                 (when (funcall pred el)
                                   (push el col))))
    col))

(defun work/org-journal-insert-tasks-heading ()
  (let ((journal-headings (work/get-journal-headings "Tasks")))
    (unless (member "Tasks" journal-headings)
      (goto-char (point-max))
      (insert "** Tasks [/]")
      (org-entry-put (point-max) "LOGGING" "STRT(!) DONE(!) logdone"))))

(defun work/valid-number (text)
  "Returns whether or not TEXT is a valid integer."
  (not (eq nil (string-match-p (rx (one-or-more digit)) text))))

(defun work/get-org-journal-entries-since-start-date ()
  "Returns list of org-journal entries since start date."
  (let ((entries-with-date (mapcar* #'cons (org-journal--list-dates) (org-journal--list-files))))
    (cl-remove-if-not #'work/entry-since-start-date-p entries-with-date)))

(defun work/get-word-at-point-or-region ()
  "Returns either the word at point or the contents of the region."
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (start (car bounds))
         (end (cdr bounds)))
    `((bounds . ,bounds)
      (text . ,text)
      (start . ,start)
      (end . ,end))))

(defun work/get-previous-org-journal-date ()
  "Returns the last org-journal date that is not today."
  (let ((today (encode-time (decode-time))))
    (car (last (seq-filter
                (lambda (date)
                  (not (string= (format-time-string org-journal-file-format date)
                                (format-time-string org-journal-file-format today))))
                (work/get-org-journal-dates-encoded t))))))

(defun work/get-org-journal-dates-encoded (&optional sort)
  "Returns all org-journal dates mapped to `encode-time`.''"
  (let ((dates (mapcar
                (lambda (date) (seq-let (month day year) date
                                 (encode-time 0 0 0 day month year)))
                (org-journal--list-dates))))
    (if sort
        (sort dates 'time-less-p)
      dates)))

(defun work/org-journal-after-header-create-hook ()
  (work/journal-add-default-headers))

(defun work/org-journal-file-header (&rest args)
  "Stuff to add to org journal file header."
  (concat
   "#+options: toc:nil author:nil e:nil\n\n"))

(defun work/journal-add-default-headers ()
  ;; (when (null (work/journal-contains-task-header-p))
  ;;   (work/insert-task-header))
  (save-excursion
    (goto-char (point-max))
    (insert "\n** Notes\n")))

(defun work/journal-contains-task-header-p ()
  (save-excursion
    (goto-char (point-min))
    (not (null (re-search-forward (rx "** Task") nil t)))))

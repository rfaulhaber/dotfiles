;;; ../Sources/dotfiles/doom.d/scripts/work.el -*- lexical-binding: t; -*-

;; defines work-related functions, i.e. stuff related to getting my day job done

(require 'request)

(defconst work/tasks-pattern (rx "Tasks" (zero-or-more any)))
(defconst work/jira-board-template "https://onekfinancial.atlassian.net/jira/software/projects/IE/boards/43?selectedIssue=IE-%s")
(defconst work/logging-properties "STRT(!) QC(!) DPLY(!) DONE(!) logdone")

(defvar work/email nil "Work email. Defaults to nil for privacy reasons.")

(defconst work/org-todo-items
  '((sequence "TODO(t)" "STRT(s)" "QC(q)" "DPLY(e)" "|" "DONE(d)" "BLOCKED(b)")
    (sequence "[ ](T)" "[-](S)" "[?](Q)" "[!](E)" "|" "[X](D)" "[B](B)"))
  "Custom work TODO items.")

(defconst work/org-carryover-items "TODO=\"TODO\"|TODO=\"STRT\"|TODO=\"QC\"|TODO=\"DPLY\""
  "Custom carryover items")

(defconst work/hl-todo-faces
  '(("TODO" warning bold)
    ("STRT" font-lock-constant-face bold)
    ("DPLY" font-lock-keyword-face bold)
    ("DONE" font-lock-doc-face bold)
    ("BLOCKED" error bold)))

;; TODO add function that converts bullet item to TODO heading

;; TODO make more robust jira connection
;; (defun work/org-link-number-to-jira-ticket ()
;;   "Replaces the number under point with a link to the related ticket in jira."
;;   (interactive)
;;   (when (or (eq major-mode 'org-mode) (eq major-mode 'org-journal-mode))
;;     (let* ((bound-info (work/get-word-at-point-or-region))
;;            (bounds     (cdr (assoc 'bounds bound-info)))
;;            (text       (cdr (assoc 'text bound-info)))
;;            (start      (cdr (assoc 'start bound-info)))
;;            (end        (cdr (assoc 'end bound-info))))
;;       (when (and bounds (work/valid-number text))
;;         (delete-region start end)
;;         (insert (format work/jira-link-template text text))))))

;; TODO
;; (defun work/create-git-branch-from-bitbucket-ticket ()
;;   (interactive))

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

(defun work/insert-blank-standup-header ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (insert "** Standup
*** Yesterday 🌝
**** Standup
-
**** What did I miss?
**** Why did I miss?
*** Today 🌞
-
*** On track for this week: 🟢
")))

(defun work/make-work-standup-template ()
  (interactive)
  (let ((open-items (work/get-in-progress-org-items-for-buffer)))
    (save-excursion
      (goto-char (point-max))
      (apply #'insert "** Standup\n" (mapcar (lambda (item)
                                               (format "- %s:\n" item))
                                             open-items)))))

(defun work/get-in-progress-org-items-for-buffer ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "** Tasks")
    (seq-filter #'identity
                (org-map-entries (lambda ()
                                   (when (eq (org-element-property :todo-type (org-element-at-point)) 'todo)
                                     (let ((title (org-element-property :title (org-element-at-point))))
                                       (if (string-match (rx "[[" (one-or-more any) "][" (group (one-or-more any)) "]]") title)
                                           (match-string 1 title)
                                         title))))
                                 nil 'tree))))

(defun work/get-random-number-of-length (len)
  (substring (number-to-string (abs (random))) 0 len))

(defun work/get-random-phone-number (&optional prefix)
  "Returns a number that could look like a phone number."
  (interactive "p")
  (let ((number (work/get-random-number-of-length 10)))
    (pcase prefix
      (1 (insert number))
      (4 (progn
           (kill-new number)
           (message "%s has been added to the kill-ring" number))))))

(defun work/get-random-work-email (&optional prefix)
  (interactive "p")
  (let* ((number (work/get-random-number-of-length 10))
         (email (format "ryan.faulhaber+testing%s@facetwealth.com" number)))
    (pcase prefix
      (1 (insert email))
      (4 (progn
           (kill-new email)
           (message "%s has been added to the kill-ring" email))))))

(defun work/insert-jira-story (story-number)
  (interactive "nStory number: ")
  (let ((link (format work/jira-board-template story-number)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (regexp-quote "Tasks"))
      (org-insert-subheading t)
      (insert (format "TODO [[%s][IE-%s]]" link story-number)))))

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

(defun work/get-standup-heading-org-element (data)
  (work/org-element-filter data 'headline (lambda (el)
                                            (string=
                                             (org-element-property :title el)
                                             "Standup"))))

(defun work/org-journal-insert-standup-template ()
  "Inserts standup template in org journal file."
  (let ((previous-journal-entry (car (reverse (org-journal--list-files)))))
    (message "previous %s" previous-journal-entry)
    (with-temp-buffer
      (insert-file-contents previous-journal-entry)
      (let* ((standup-heading (car (org-element-map (org-element-parse-buffer) 'headline
                                     (lambda (headline)
                                       (when (and
                                              (eq (org-element-property :level headline) 2)
                                              (string= (org-element-property :raw-value headline) "Standup"))
                                         (list (org-element-property :begin headline)
                                               (org-element-property :end headline)))))))
             (start (car standup-heading))
             (end (cadr standup-heading)))
        (kill-region start end))))
  (goto-char (point-max))
  (insert "\n")
  (yank)
  (message "finished"))

(defun work/org-journal-insert-tasks-heading ()
  (message "binding")
  (let ((journal-headings (work/get-journal-headings "Tasks")))
    (message "headings %s" journal-headings)
    (unless (member "Tasks" journal-headings)
      (goto-char (point-max))
      (insert "** Tasks [/]")
      (org-entry-put (point-max) "LOGGING" "STRT(!) REVIEW(!) DONE(!) logdone"))))

(defun work/valid-number (text)
  "Returns whether or not TEXT is a valid integer."
  (not (eq nil (string-match-p (rx (one-or-more digit)) text))))

(defun work/get-org-journal-entries-since-start-date ()
  "Returns list of org-journal entries since start date."
  (let ((entries-with-date (mapcar* #'cons (org-journal--list-dates) (org-journal--list-files))))
    (cl-remove-if-not #'work/entry-since-start-date-p entries-with-date)))

(defun work/entry-since-start-date-p (entry)
  "Returns whether or not an org-journal entry was created since the start date."
  (let* ((date (car entry))
         (entry-month (car date))
         (entry-day (nth 1 date))
         (entry-year (nth 2 date))
         (start-month (car work/start-standup-date))
         (start-day (nth 1 work/start-standup-date))
         (start-year (nth 2 work/start-standup-date)))
    (cond
     ((= start-year entry-year) (and (>= entry-month start-month) (>= entry-day start-day)))
     (t (> entry-year start-year)))))

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

(defun work/get-standup-heading ()
  "Returns standup heading from org-journal entry. Must be run inside of org-journal mode."
  (car
   (seq-filter
    (lambda (entry)
      (not (null entry)))
    (org-map-entries (lambda ()
                       (let* ((el (org-element-at-point))
                              (level (org-element-property :level el))
                              (title (org-element-property :title el)))
                         (when (and
                                (= level 2)
                                (string= title "Standup"))
                           el)))))))

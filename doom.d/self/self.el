;;; ~/.doom.d/scripts/self.el -*- lexical-binding: t; -*-

;; this file is used for miscellaneous useful functions that I've written to
;; make Emacs easier to use for myself. They are not quite large enough or
;; coherent to make a module or package.

(defvar self/dict "~/.dict" "A path to a personal word list, such as /usr/share/dict/words")
(defvar self/common-directories '() "Alist (Name . Path) of common directories, used by self/visit-common-directories")

(defconst self/date-format-options '(("MM/YYYY"    . "%m/%Y")
                                     ("MM/DD"      . "%m/%d")
                                     ("MM/DD/YYYY" . "%m/%d/%Y"))
  "Various date formats used in interactive functions.")

;; TODO break this up into separate files!

;; -------------------- interactive functions ----------------------------------

(defun self/eww-open-url-window-right (url)
  "Opens URL in eww-mode in a new window to the right."
  (interactive "sURL: ")
  (self/open-eww-window url))

(defun self/eww-open-url-from-clipboard ()
  "Opens URL from clipboard in eww-mode in a new window to the right."
  (interactive)
  (self/open-eww-window (current-kill 0)))

(defun self/open-eww-window (url)
  (with-selected-window (split-window-right)
    (switch-to-buffer "*webpage*")
    (eww-mode)
    (eww url)))

;; thank you github.com/hrs for the inspiration
(defun self/new-scratch-buffer ()
  "Creates and opens a new scratch buffer with a random name"
  (interactive)
  (let ((new-window (split-window-below)))
    (with-selected-window new-window
      (switch-to-buffer (format "*%s*" (make-temp-name "scratch-"))))))

(defun self/org-roam-subtree-to-new-file ()
  "Moves current Org subtree to new org-roam file. Kind of hacky!"
  (interactive)
  (let* ((el (org-element-at-point))
         (title (org-element-property :title el))
         (type (car el))
         (header-start (org-element-property :begin el))
         (el-start (org-element-property :contents-begin el))
         (el-end (org-element-property :contents-end el)))
    (if (not (eq 'headline type))
        (user-error "Not at header")
      (progn
        (kill-region el-start el-end)
        (goto-char header-start)
        (kill-line)
        (let ((new-buf (with-temp-buffer
                         (org-mode)
                         (insert "\n")
                         (insert (car (cdr kill-ring)))
                         (buffer-string))))
          (org-roam-insert nil (list title) nil title))
        (insert new-buf)))))

(defun self/org-paste-quote (page-number)
  "Inserts latest element of kill ring into quote block."
  ;; TODO automatically insert into document
  (interactive "sPage number: ")
  (insert (format "- %s\n" page-number))
  (insert "\t#+begin_quote\n\t")
  (yank)
  (insert "\n\t#+end_quote"))

;; thank you xah
(defun self/unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;; TODO check if in calendar-mode first
;; TODO refactor next two functions
(defun self/calendar-insert-date ()
  "Capture the date at point, exit the Calendar, insert the date."
  (interactive)
  (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
    (calendar-exit)
    (let* ((date (encode-time 0 0 0 day month year))
           (options '(("MM/DD"      . "%m/%d")
                      ("MM/DD/YYYY" . "%m/%d/%Y")))
           (option (completing-read "Select a format: " (mapcar 'car options)))
           (output (format-time-string (cdr (assoc option options)) date)))
      (insert output))))

(defun self/insert-current-date-at-point ()
  "Inserts date at point in the chosen format."
  (interactive)
  (let* ((output (self/format-date-from-option (self/choose-date-format))))
    (with-current-buffer (current-buffer)
      (insert output))))

(defun self/find-org-file ()
  "Search for a file in `org-directory'."
  (interactive)
  (self/find-file-non-recursive "~/org"
                                :exclude-directories t
                                :filter-fn (lambda (file)
                                             (not
                                              (string-match-p (rx (seq any "_archive")) file)))))

;; TODO refactor next two functions
(defun self/org-roam-find-files-created-today ()
  "Returns a list of files under the org roam directory that were created today."
  (interactive)
  (let* ((today-date-format (format-time-string "%Y%m%d"))
         (org-files (org-roam--directory-files-recursively org-roam-directory (format "%s.*" today-date-format)))
         (input-choice (completing-read "Select file: " org-files)))
    (find-file input-choice)))

(defun self/org-roam-find-files-for-date ()
  "Returns a list of files under the org roam directory for selected date."
  (interactive)
  (when (string= major-mode "calendar-mode")
    (let* ((date (format-time-string "%Y%m%d" (self/get-date-from-calendar)))
           (org-files (org-roam--directory-files-recursively org-roam-directory (format "%s.*" date)))
           (input-choice (completing-read "Select file: " org-files)))
      (find-file input-choice))))

(defun self/dired-here ()
  "Opens a dired buffer in the current directory."
  (interactive)
  (dired "."))

(defun self/mu4e-load-path-fix ()
  "This is a workaround for when Doom / Emacs does not add mu4e to the load path."
  (interactive)
  (let ((path (string-trim (shell-command-to-string "fd --type d 'mu4e' /nix/store"))))
    (unless (member path load-path)
      (add-to-list 'load-path path))))

(defun self/copy-line-number-reference (arg)
  (interactive "p")
  (when-let ((file-name (buffer-file-name)))
    (pcase arg
      (1 (kill-new (format "%s:%s" file-name (line-number-at-pos))))
      (4 (kill-new (format "%s:%s:%s" file-name (line-number-at-pos) (current-column)))))))

;; thank you EmacsWiki
(defun self/sort-words-in-region (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun self/visit-common-directories ()
  "Open a common directory in Dired. `self/common-directories' must be set first."
  (interactive)
  (let ((names (mapcar 'car self/common-directories)))
    (if (eq names nil)
        (user-error "variable self/common-directories is not set")
      (let ((selection (ivy-read "Select a directory: " names)))
        (dired (cdr (assoc selection self/common-directories)))))))

(defun self/suspend-save ()
  "A stupid hack to allow for things like saving without formatting."
  (interactive)
  (major-mode-suspend)
  (text-mode)
  (save-buffer)
  (major-mode-restore))

(defun self/org-journal-open-last-entry ()
  "Opens last org-journal entry"
  (interactive)
  (find-file (car (reverse (org-journal--list-files)))))

(defun self/rename-this-file (new-name)
  "Renames the current file to NEW-NAME."
  (interactive "sNew name: ")
  (rename-file (file-name-nondirectory (buffer-file-name)) new-name)
  (kill-buffer)
  (switch-to-buffer (find-file-noselect new-name)))

(defun self/fill-line-length-with-character (char &optional direction)
  "Inserts a line of CHAR of current line length above and below the current line. One prefix argument only adds the bottom line, and two prefix arguments only adds the top line."
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

;; -------------------- utility functions ---------------------------------------

;; stolen from https://gitlab.com/ngm/commonplace-lib/-/blob/master/commonplace-lib.el
;; thank you Neil
(defun self/slugify-title (title)
  "Convert TITLE to a filename-suitable slug.  Use hyphens rather than underscores."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("['\?,%]" . "")
                    ("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                    ("--*" . "-")  ;; remove sequential underscores
                    ("^-" . "")  ;; remove starting underscore
                    ("-$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))


;; stolen from https://gitlab.com/ngm/commonplace/-/blob/master/publish-agora.el
;; thank you Neil
(defun self/get-title (file)
  "For a given file, get its TITLE keyword."
  (with-current-buffer
      (get-file-buffer file)
    (cadar (org-collect-keywords '("TITLE")))))

;; stolen from https://gitlab.com/ngm/commonplace/-/blob/master/publish-agora.el
;; with one minor change
;; thank you Neil
;; TODO refactor into ox-agora fork
(defun self/slugify-export-output-file-name (output-file)
  "Gets the title of the org file and uses this (slugified) for the output
filename. This is mainly to override org-roam's default filename convention of
`timestamp-title_of_your_note` which doesn't work well with Agora."
  (if (org-roam-file-p)
      (let* ((title (self/get-title (buffer-file-name (buffer-base-buffer))))
             (directory (file-name-directory output-file))
             (slug (self/slugify-title title)))
        (concat directory slug ".md"))
    output-file))


;; this comes from reddit. thank you r/emacs!
(defun self/org-md-paragraph-unfill (&rest args)
  "Unfill CONTENTS, the `cadr' in ARGS."
  (let* ((actual-args (car args))
         (org-el (nth 0 actual-args))
         (contents (nth 1 actual-args))
         (info (nth 2 actual-args)))
    ;; Unfill contents
    (unless (eq (car org-el) 'src-block)
      (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
    (list org-el contents info)))

(defun self/capture-insert-file-link ()
  "Imitation of org-insert-link but for use in org-capture template"
  (let* ((file-path (read-file-name "File: "))
         (file-name (read-from-minibuffer "Description: ")))
    (format "[[%s][%s]]" file-path file-name)))


(defun self/pick-random-word (word-count)
  "Picks WORD-COUNT number of random words from the system dictionary."
  (if (and
       (boundp 'self/dict)
       (file-exists-p self/dict)
       (not (eq self/dict nil)))
      (let* ((lines (s-split "\n" (self/slurp self/dict) t))
             (line-len (length lines))
             (words '()))
        (dotimes (i word-count)
          (let ((num (random (- line-len 1))))
            (push (nth num lines) words)))
        words)
    (user-error "self/dict is not defined")))

;; thank you github: https://github.com/bcbcarl/emacs-wttrin/issues/16#issuecomment-658987903
(defun self/wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY."
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://wttr.in/" query "?A")
         (lambda (status) (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

;; thank you doom emacs discord user zzamboni
;; https://discordapp.com/channels/406534637242810369/695219268358504458/788524346309214249
(defun self/org-md-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (or (org-element-property :language src-block) "")))
    (format "```%s\n%s```\n"
            lang
            (org-remove-indentation
             (org-export-format-code-default src-block info)))))

(defun self/write-temp (beg end &optional prefix)
  "Writes BEG and END from current buffer into a temporary file."
  (with-current-buffer (current-buffer)
    (let ((temp-path (self/mktemp prefix)))
      (write-region beg end temp-path))))

(defun self/mktemp (&optional prefix)
  "Calls mktemp and passes PREFIX to the command, defaulting to 'emacs'. Returns result of `mktemp`."
  (let ((pre
         (cond
          ((null prefix) "emacsXXX")
          ((not (s-contains-p "XXX" prefix)) (format "%sXXX" prefix))
          (t prefix))))
    (s-trim-right (shell-command-to-string (format "mktemp -p /tmp %s" pre)))))

;; TODO write more generic roam exporter that extends org publishing
(defun self/org-export-preprocessor (_backend)
  "For org-roam files, this will append all backlinks to a file to the end."
  (when (org-roam-file-p)
    (let ((links (mapcar
                  (lambda (backlink)
                    (let* ((source-node (org-roam-backlink-source-node backlink))
                           (source-title (org-roam-node-title source-node))
                           (source-id (org-roam-node-id source-node)))
                      (format " - [[id:%s][%s]]\n" source-id source-title)))
                  (org-roam-backlinks-get (org-roam-node-at-point)))))
      (unless (eq (length links) 0)
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") (apply 'concat links)))))))

;; TODO see above todo
(defun self/org-roam-export-refs (_backend)
  "For org-roam files, exports the ROAM_REF property as a section at the bottom of the file as an unordered list."
  (save-excursion
    (goto-char (point-min))
    (when (and
           (org-roam-file-p)
           (not (eq (assoc "ROAM_REFS" (org-entry-properties)) nil)))
      (goto-char (point-min))
      (let* ((file-refs (split-string (cdr (assoc "ROAM_REFS" (org-entry-properties))) " "))
             (refs-as-bullet-links (mapcar
                                    (lambda (link)
                                      (format "- [[%s]]\n" link))
                                    file-refs)))
        (unless (or
                 (eq refs-as-bullet-links nil)
                 (eq (length refs-as-bullet-links) 0))
          (goto-char (point-max))
          (insert (concat "\n* Refs\n") (apply 'concat refs-as-bullet-links)))))))

;; thank you stackoverflow
(defun self/slurp (f)
  "Like Clojure's slurp; reads a file to a value."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun self/get-date-from-calendar ()
  "Returns encoded time of date under point in calendar-mode."
  (when (string= major-mode "calendar-mode")
    (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
      (calendar-exit)
      (encode-time 0 0 0 day month year))))

(cl-defun self/find-file-non-recursive (dir &key prompt filter-fn exclude-directories show-hidden)
  "Like `counsel-find-file' for DIR, but excludes directories and their children.
PROMPT sets the `ivy-read' prompt.
FILTER-FN is a function to filter the list of retrieved files from the directory.
EXCLUDE-DIRECTORIES, if non-nil, will remove any directories from the list.
If SHOW-HIDDEN is non-nil, will include any files that begin with ."
  (let* ((dir (concat (string-trim-right dir (rx (one-or-more "/"))) "/"))
         (filter (rx line-start (not ".") (zero-or-more any) eol)) ; ^[^.].*$
         (files (directory-files dir nil (if show-hidden nil filter)))
         (filtered-files (if filter-fn (seq-filter filter-fn files) files))
         (non-dir-files (if exclude-directories (seq-filter (lambda (file)
                                                              (not
                                                               (file-directory-p
                                                                (concat dir "/" file))))
                                                            filtered-files)))
         (selection (ivy-read (or prompt "Find file: ") non-dir-files))
         (file-name (concat dir selection)))
    (find-file file-name)))

(defun self/org-insert-modified-timestamp ()
  "Inserts inactive timestamp to bottom of file."
  (when (org-roam--org-roam-file-p)
    (goto-char (point-max))
    (insert "Updated: ")
    (org-time-stamp '(16) 'inactive)))

(defun self/choose-date-format ()
  "Provides user with options from `self/date-format-options'."
  (completing-read "Select a format: " (mapcar 'car self/date-format-options)))

(defun self/format-date-from-option (option)
  "Formats current date according to selected date option."
  (format-time-string (cdr (assoc option self/date-format-options))))

(defun self/org-filter-headings (filter-func)
  (let ((headings nil))
    (org-map-entries
     (lambda ()
       (when (funcall filter-func (org-heading-components))
         (push (org-heading-components) headings))))
    headings))

(defun self/org-property-filter (data types pred)
  "Like `org-element-map', but a filter function. Applies DATA, TYPES, and PRED to `org-element-map'"
  (let ((col nil))
    (org-element-map seq types (lambda (el)
                                 (when (funcall pred el)
                                   (push el col))))
    col))

(defun self/get-file-hierarchy-names (path level)
  "Given a PATH, gets the nth name up in the file hierarchy."
  (if (or (eq path nil)
          (not (file-name-absolute-p path)))
      (user-error "%s is nil or not an absolute path!" path)
    (let* ((file-name-components (seq-filter
                                  (lambda (str)
                                    (not (string= "" str)))
                                  (split-string (file-name-directory path) "/")))
           (up (- (length file-name-components) level)))
      (nth up file-name-components))))

;; custom evil operators ------------------------------------

(evil-define-operator self/evil-write-temp (beg end &optional prefix)
  "Like evil-write, but creates a new temporary file and writes to that."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<r><a>")
  (let ((s (or beg (point-min)))
        (f (or end (point-max)))
        (bufname (buffer-file-name (buffer-base-buffer))))
    (cond
     ((null bufname) (let ((filename (self/mktemp prefix)))
                       (write-file filename)))
     (t (self/write-temp s f prefix)))))

(evil-define-operator self/evil-write-suspend (beg end type file-or-append &optional bang)
  "Like evil-write, but quickly changes the buffer to `text-mode' first.
This is meant to skip any kind of automatic formatting."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<r><fsh><!>")
  (major-mode-suspend)
  (text-mode)
  (evil-write beg end type file-or-append bang)
  (major-mode-restore))

;; (evil-define-operator self/evil-ex-uniq (beg end)
;;   "Like :sort u, except it doesn't sort."
;;   :motion mark-whole-buffer
;;   :move-point nil
;;   (interactive "<r>")
;;   (let ((main-buf (current-buffer)))
;;     (insert (with-temp-buffer
;;               (insert-buffer-substring main-buf beg end)
;;               (sort-lines nil (point-min) (point-max))
;;               (while (and (< (point) (point-max)) (not eobp))
;;                 (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;                   )
;;                 )
;;               ))
;;     )
;;   )

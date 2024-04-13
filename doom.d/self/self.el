;;; ~/.doom.d/scripts/self.el -*- lexical-binding: t; -*-

;; this file is used for miscellaneous useful functions that I've written to
;; make Emacs easier to use for myself. They are not quite large enough or
;; coherent to make a module or package.

;; --------------------------------- variables ---------------------------------

(defvar self/dict "~/.dict" "A path to a personal word list, such as /usr/share/dict/words")
(defvar self/common-directories '() "Alist (Name . Path) of common directories, used by self/visit-common-directories")

(defconst self/date-format-options '(("MM/YYYY"    . "%m/%Y")
                                     ("MM/DD"      . "%m/%d")
                                     ("MM/DD/YYYY" . "%m/%d/%Y")
                                     ("YYYYMMDD" . "%Y%m%d"))
  "Various date formats used in interactive functions.")

;; ------------------------------ function aliases -----------------------------

;; I can never remember the envrc functions
(defalias 'direnv-allow 'envrc-allow)
(defalias 'direnv-reload 'envrc-reload)

;; TODO break this up into separate files!

;; ---------------------------- interactive functions ----------------------------

;; thank you github.com/hrs for the inspiration
(defun self/new-scratch-buffer ()
  "Creates and opens a new scratch buffer with a random name"
  (interactive)
  (let ((new-window (split-window-below)))
    (with-selected-window new-window
      (switch-to-buffer (format "*%s*" (make-temp-name "scratch-"))))))

;; thank you xah
(defun self/unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun self/calendar-insert-date ()
  "Using `org-read-date', insert the returned date."
  (interactive)
  (let* ((date (org-read-date nil t))
         (option (completing-read "Select a format: " (mapcar 'car self/date-format-options)))
         (output (format-time-string (cdr (assoc option self/date-format-options)) date)))
    (insert output)))

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
  (let* ((date (format-time-string "%Y%m%d" (org-read-date nil t)))
         (org-files (org-roam--directory-files-recursively org-roam-directory (format "%s.*" date)))
         (input-choice (completing-read "Select file: " org-files)))
    (find-file input-choice)))

(defun self/dired-here ()
  "Opens a dired buffer in the current directory."
  (interactive)
  (dired "."))

(defun self/mu4e-load-path-fix ()
  "This is a workaround for when Doom / Emacs does not add mu4e to the load path."
  (interactive)
  (let ((path (string-trim (nth 0 (split-string (shell-command-to-string "fd --type d 'mu4e' /nix/store") "\n")))))
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
      (let ((selection (completing-read "Select a directory: " names)))
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

;; thank you ChatGPT
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
      (insert (concat comment-start centered-text comment-end))
      (comment-region (line-beginning-position) (line-end-position)))))

(defun self/eww-open-here (url)
  "Opens a new EWW buffer with URL here in the current window."
  (interactive "sURL: ")
  (let ((new-buf (get-buffer-create "*webpage*")))
    (with-current-buffer new-buf
      (eww-mode)
      (eww url current-prefix-arg))
    (switch-to-buffer new-buf)))

(defun self/roam-ref-add-from-clipboard ()
  (interactive)
  (org-roam-ref-add (car kill-ring)))

;; thank you ChatGPT
(defun self/new-buffer-with-mode ()
  "Create a new buffer with a selected major mode."
  (interactive)
  (let* ((available-modes (sort (mapcar #'symbol-name (apropos-internal "-mode$" 'commandp)) #'string-lessp))
         (mode (completing-read "Enter major mode: " available-modes)))
    (switch-to-buffer (generate-new-buffer "*new*"))
    (funcall (intern mode))))

(defun self/surround-line-with-character (char)
  "Surrounds the current line with a character CHAR.
For example, give this line of text:

hello world

returns:

===========
hello world
==========="
  (interactive "sChar: ")
  (let ((current-line-length (- (eol) (bol))))
    (save-excursion
      (forward-line -1)
      (delete-region (bol) (eol))
      (insert (make-string current-line-length (string-to-char char)))
      (forward-line 2)
      (delete-region (bol) (eol))
      (insert (make-string current-line-length (string-to-char char))))))

;; thank you ChatGPT
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
(defun self/evil-ex-remove-duplicates (beg end)
  "Remove duplicate lines in the region from BEG to END."
  (interactive "r")
  (save-excursion
    (narrow-to-region beg end)
    (delete-duplicate-lines (point-min) (point-max))
    (widen)))

(defun self/projectile-open-project-in-new-workspace (&optional arg)
  (interactive "P")
  (if-let ((projects (projectile-relevant-known-projects))
           (selected-project (completing-read "Select a project: " projects nil t))
           (selected-project-name (f-filename selected-project)))
      (progn
        (+workspace-switch selected-project-name t)
        (projectile-switch-project-by-name selected-project)
        (+workspace/display))
    (user-error "Something is wrong with projectile config!")))

;; ----------------------------- utility functions -----------------------------

;; stolen from https://gitlab.com/ngm/commonplace-lib/-/blob/master/commonplace-lib.el
;; thank you Neil
(defun self/slugify-title (title)
  "Convert TITLE to a filename-suitable slug. Use hyphens rather than underscores."
  (cl-flet* ((nonspacing-mark-p (char)
               (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
               (apply #'string (seq-remove #'nonspacing-mark-p
                                           (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
               (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("['\?,%]" . "")
                    ("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                    ("--*" . "-")                   ;; remove sequential underscores
                    ("^-" . "")                     ;; remove starting underscore
                    ("-$" . "")))                   ;; remove ending underscore
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
             (slug (self/slugify-title title))
             (ext (url-file-extension output-file)))
        (concat directory slug ext))
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
  "For org-roam files, exports the ROAM_REF property as a section at the bottom
of the file as an unordered list."
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

(cl-defun self/find-file-non-recursive (dir &key prompt filter-fn exclude-directories show-hidden)
  "Like `counsel-find-file' for DIR, but excludes directories and their
children. PROMPT sets the `completing-read' prompt. FILTER-FN is a function to
filter the list of retrieved files from the directory. EXCLUDE-DIRECTORIES, if
non-nil, will remove any directories from the list. If SHOW-HIDDEN is non-nil,
will include any files that begin with ."
  (let* ((dir (concat (string-trim-right dir (rx (one-or-more "/"))) "/"))
         (filter (rx line-start (not ".") (zero-or-more any) eol)) ; ^[^.].*$
         (files (directory-files dir nil (if show-hidden nil filter)))
         (filtered-files (if filter-fn (seq-filter filter-fn files) files))
         (non-dir-files (if exclude-directories (seq-filter (lambda (file)
                                                              (not
                                                               (file-directory-p
                                                                (concat dir "/" file))))
                                                            filtered-files)))
         (selection (completing-read (or prompt "Find file: ") non-dir-files))
         (file-name (concat dir selection)))
    (find-file file-name)))

(defun self/org-insert-modified-timestamp ()
  "Inserts inactive timestamp to bottom of file."
  (when (org-roam--org-roam-file-p)
    (save-excursion
      (goto-char (point-max))
      (insert "Updated: ")
      (org-time-stamp '(16) 'inactive))))

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
  "Like `org-element-map', but a filter function. Applies DATA, TYPES, and PRED
to `org-element-map'"
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

(defun self/swap-lines (left right)
  "Swaps line at line number LEFT with RIGHT."
  (let ((left-line-contents (self/get-line-contents left))
        (right-line-contents (self/get-line-contents right)))
    (self/goto-line-non-interactive left)
    (kill-line)
    (insert right-line-contents)
    (self/goto-line-non-interactive right)
    (kill-line)
    (insert left-line-contents)))

(defun self/get-line-contents (line-number)
  "Returns the contents of a line on LINE-NUMBER."
  (self/goto-line-non-interactive line-number)
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun self/goto-line-non-interactive (line-number)
  "Helper for going to a line at LINE-NUMBER without invoking `goto-line'."
  (forward-line (- line-number (line-number-at-pos))))

(defun self/goto-col-non-interactive (col-number)
  "Helper for going to a col at COL-NUMBER without invoking `goto-char' or
`move-to-column'."
  (forward-char (- col-number (current-column))))

;; thank you http://stackoverflow.com/questions/6158990/generating-randoms-numbers-in-a-certain-range-for-common-lisp
(defun self/random-in-range (start end)
  "Returns a random number n where START <= n <= END."
  (+ start (random (+ 1 (- end start)))))

(defun self/org-publish-before-advice (&rest args)
  (org-roam-update-org-id-locations))

(defun self/lookup-open-link-like-object (lookup-fn &rest args)
  "Advice for LOOKUP-FN. Opens a link-like object: a file, URL, etc."
  (let ((identifier (nth 0 args))
        (url-pattern (rx line-start (seq "http" (? "s") "://")))
        (file-path-pattern (rx line-start (group (one-or-more any)) "/" (group (one-or-more (not "/"))) line-end)))
    (cond
     ((string-match-p url-pattern identifier) (browse-url identifier))
     ((and
       (string-match-p file-path-pattern identifier)
       (string-match-p ":" identifier))
      (self/open-path-with-line-and-col identifier))
     ((file-directory-p identifier) (dired identifier))
     ((file-exists-p identifier) (switch-to-buffer (find-file-noselect identifier)))
     (t (apply lookup-fn args)))))

(defun self/open-path-with-line-and-col (path)
  (seq-let (file-name line col) (split-string path ":")
    (self/open-file-at-line-number file-name (string-to-number line) (string-to-number col))))

(defun self/open-file-at-line-number (path line &optional col)
  "Opens file at PATH at line number LINE, and optionally COL. If COL > length
of line, moves cursor to the end of LINE."
  (when current-prefix-arg
    (select-window (split-window (selected-window) nil (pcase current-prefix-arg
                                                         ((or '(4) 4) 'right)
                                                         (1 'down)
                                                         (2 'up)
                                                         (3 'left)))))
  (switch-to-buffer (find-file-noselect path))
  (self/goto-line-non-interactive line)
  (when col
    (if (< (- (line-end-position) (point)) col)
        (end-of-line)
      (self/goto-col-non-interactive col))))

(defun self/shuffle (lst)
  "Shuffles a list LST."
  (let ((n (length lst)))
    (dotimes (i (length lst))
      (let ((j (+ i (random (- n i)))))
        (when (/= i j)
          (cl-rotatef (elt lst i) (elt lst j))))))
  lst)

(defun self/pick (lst)
  "Randomly chooses an item from a list."
  (let ((choice (abs (random (length lst)))))
    (nth choice lst)))

;; NOTE doesn't quite work?
(defun self/org-babel-execute-src-block-lazy-load (original-fn &rest args)
  (let ((lang (org-element-property :language (org-element-at-point))))
    (when (or (string= lang "bash") (string= lang "sh"))
      (setq lang "shell"))
    (unless (or (not (boundp 'org-babel-load-languages)) (cdr (assoc (intern lang) org-babel-load-languages)))
      (add-to-list 'org-babel-load-languages (cons (intern lang) t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    (apply original-fn args)))

(defun self/+org-inline-image-data-fn (_original-fn &rest args)
  (cl-destructuring-bind (_ link) args
    (with-demoted-errors "%S" (base64-decode-string link))))

;; --------------------------- custom evil operators ---------------------------

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
     ((null bufname) (let ((filename (make-temp-file prefix)))
                       (write-file filename)))
     (t (let ((tmpfile (make-temp-file prefix))
              (write-region s f tmpfile)))))))

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

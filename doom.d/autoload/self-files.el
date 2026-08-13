;;; $DOOMDIR/autoload/self-files.el -*- lexical-binding: t; -*-

;; commands for finding and opening files, directories, projects, and buffers

(defvar self/common-directories '() "Alist (Name . Path) of common directories, used by self/visit-common-directories")

(defvar self/global-config-file-path "~/.config/globals.json" "Path to personal global config.")

(defvar self/external-terminal-command "ghostty"
  "The terminal emulator program to use for `self/open-external-terminal-for-current-project'.")

(defvar self/external-terminal-args-fn
  (lambda (dir)
    (list (format "--working-directory=%s" dir)))
  "Function that takes a directory and returns a list of arguments for `self/external-terminal-command'.")

(defvar self/dotfiles-location "~/Projects/dotfiles"
  "Location of dotfiles.")

;; thank you github.com/hrs for the inspiration
;;;###autoload
(defun self/new-scratch-buffer ()
  "Creates and opens a new scratch buffer with a random name"
  (interactive)
  (let ((new-window (split-window-below)))
    (with-selected-window new-window
      (switch-to-buffer (format "*%s*" (make-temp-name "scratch-"))))))

;;;###autoload
(defun self/find-org-file ()
  "Search for a file in `org-directory'."
  (interactive)
  (self/find-file-non-recursive "~/org"
                                :exclude-directories t
                                :filter-fn (lambda (file)
                                             (not
                                              (string-match-p (rx "_archive") file)))))

;;;###autoload
(defun self/find-org-file-dir ()
  "Find file or folder in `org-directory'"
  (interactive)
  (find-file org-directory))

;; TODO refactor next two functions
;;;###autoload
(defun self/org-roam-find-files-created-today ()
  "Returns a list of files under the org roam directory that were created today."
  (interactive)
  (let* ((today-date-format (format-time-string "%Y%m%d"))
         (org-files (org-roam--directory-files-recursively org-roam-directory (format "%s.*" today-date-format)))
         (input-choice (completing-read "Select file: " org-files)))
    (find-file input-choice)))

;;;###autoload
(defun self/org-roam-find-files-for-date ()
  "Returns a list of files under the org roam directory for selected date."
  (interactive)
  (let* ((date (format-time-string "%Y%m%d" (org-read-date nil t)))
         (org-files (org-roam--directory-files-recursively org-roam-directory (format "%s.*" date)))
         (input-choice (completing-read "Select file: " org-files)))
    (find-file input-choice)))

;;;###autoload
(defun self/dired-here ()
  "Opens a dired buffer in the current directory."
  (interactive)
  (dired "."))

;;;###autoload
(defun self/visit-common-directories ()
  "Open a common directory in Dired. `self/common-directories' must be set first."
  (interactive)
  (let ((names (mapcar 'car self/common-directories)))
    (if (eq names nil)
        (user-error "variable self/common-directories is not set")
      (let ((selection (completing-read "Select a directory: " names)))
        (dired (cdr (assoc selection self/common-directories)))))))

;;;###autoload
(defun self/org-journal-open-last-entry ()
  "Opens last org-journal entry"
  (interactive)
  (find-file (car (reverse (org-journal--list-files)))))

;;;###autoload
(defun self/rename-this-file (new-name)
  "Renames the current file to NEW-NAME."
  (interactive "sNew name: ")
  (rename-file (file-name-nondirectory (buffer-file-name)) new-name)
  (kill-buffer)
  (switch-to-buffer (find-file-noselect new-name)))

;; thank you ChatGPT
;;;###autoload
(defun self/new-buffer-with-mode ()
  "Create a new buffer with a selected major mode."
  (interactive)
  (let* ((available-modes (sort (mapcar #'symbol-name (apropos-internal "-mode$" 'commandp)) #'string-lessp))
         (mode (completing-read "Enter major mode: " available-modes)))
    (switch-to-buffer (generate-new-buffer "*new*"))
    (funcall (intern mode))))

;;;###autoload
(defun self/projectile-open-project-in-new-workspace (&optional _arg)
  (interactive "P")
  (if-let* ((projects (projectile-relevant-known-projects))
            (selected-project (completing-read "Select a project: " projects nil t))
            (selected-project-name (f-filename selected-project)))
      (progn
        (+workspace-switch selected-project-name t)
        (projectile-switch-project-by-name selected-project)
        (+workspace/display))
    (user-error "Something is wrong with projectile config!")))

;;;###autoload
(defun self/open-org-workspace ()
  (interactive)
  (if (+workspace-exists-p "org")
      (+workspace-switch "org")
    (+workspace/new-named "org")
    (find-file org-directory)
    (+workspace-switch "org"))
  (+workspace/display))

;;;###autoload
(defun self/open-current-buffer-in-browser ()
  (interactive)
  (if-let* ((filename (buffer-file-name)))
      (browse-url filename)
    (user-error "Buffer is not associated with a file")))

;;;###autoload
(defun self/reload-projectile-projects ()
  "Reloads projectile projects from the ~/Projects directory"
  (interactive)
  (dolist (dir (directory-files "~/Projects" t directory-files-no-dot-files-regexp))
    (when (file-directory-p dir)
      (projectile-add-known-project dir))))

;;;###autoload
(defun self/paste-to-file (name)
  (interactive "FName? ")
  (get-buffer-create name)
  (with-current-buffer name
    (insert (current-kill 0))
    (write-region (point-min) (point-max) name)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun self/dired-diff-marked-files ()
  (interactive)
  (when (not (eq major-mode #'dired-mode))
    (user-error "Can only be run in Dired mode"))

  (let ((files (dired-get-marked-files)))
    (when (> 2 (length files))
      (user-error "Need at least two files to diff!"))

    (let ((buffer (get-buffer-create "*dired diff*")))
      (with-current-buffer buffer
        (erase-buffer)
        (call-process "delta" nil t nil (car files) (nth 1 files))
        (goto-char (point-min)))
      (pop-to-buffer buffer))))

;;;###autoload
(defun self/display-theme-colors ()
  "Loads and displays the theme values from `self/global-config-file-path'"
  (interactive)
  (if-let* ((buffer (get-buffer "*theme*")))
      (switch-to-buffer buffer)
    (with-current-buffer (get-buffer-create "*theme*")
      (erase-buffer)
      (when (not (file-exists-p self/global-config-file-path))
        (user-error "No global config file found!"))
      (let ((theme-output (shell-command-to-string (format "open %s | get colors.theme" self/global-config-file-path))))
        (insert theme-output)
        (rainbow-mode 1)
        (read-only-mode 1)
        (switch-to-buffer (current-buffer))))))

;;;###autoload
(defun self/open-external-terminal-for-current-project ()
  (interactive)
  (if-let* ((location (or (projectile-project-root)
                          (when buffer-file-name (f-dirname buffer-file-name)))))
      (make-process
       :name "terminal spawn"
       :buffer (get-buffer-create "*terminal spawn*")
       ;; Ghostty forwards --working-directory verbatim into the child's
       ;; $env.PWD, and nushell refuses to start when PWD has a trailing
       ;; slash — strip projectile's trailing slash and expand ~.
       :command (cons self/external-terminal-command
                      (funcall self/external-terminal-args-fn
                               (directory-file-name (expand-file-name location)))))
    (user-error "cannot find a directory to open")))

;;;###autoload
(defun self/open-projectile-project-in-new-frame (&optional _arg)
  "Like `self/projectile-open-project-in-new-workspace', but opens a new frame too."
  (interactive "P")
  (if-let* ((projects (projectile-relevant-known-projects))
            (selected-project (completing-read "Select a project: " projects nil t))
            (selected-project-name (f-filename selected-project)))
      (progn
        (let ((new-frame (make-frame)))
          (with-selected-frame new-frame
            (+workspace-switch selected-project-name t)
            (projectile-switch-project-by-name selected-project)
            (+workspace/display))))
    (user-error "Something is wrong with projectile config!")))

;;;###autoload
(defun self/find-file-in-private-config ()
  "Like `doom/find-file-in-private-config', but relative to my own dotfiles."
  (interactive)
  (unless self/dotfiles-location
    (user-error "No dotfiles location set!"))
  (unless (file-directory-p self/dotfiles-location)
    (error "Directory %S does not exist" self/dotfiles-location))
  (unless (file-readable-p self/dotfiles-location)
    (error "Directory %S isn't readable" self/dotfiles-location))
  ;; Mirrors `projectile-find-file-in-directory', but restricted to Elisp.
  ;; Going through projectile (rather than `doom-project-find-file') keeps the
  ;; listing on `git ls-files' instead of project.el's external `find' fallback.
  (let* ((dir (expand-file-name self/dotfiles-location))
         (default-directory dir)
         (files (seq-filter (lambda (f) (string-suffix-p ".el" f))
                            (projectile-dir-files dir)))
         (file (projectile-completing-read "Find Elisp file: " files)))
    (find-file (expand-file-name file dir))
    (run-hooks 'projectile-find-file-hook)))

(cl-defun self/find-file-non-recursive (dir &key prompt filter-fn exclude-directories show-hidden)
  "Like `counsel-find-file' for DIR, but excludes directories and their
children. PROMPT sets the `completing-read' prompt. FILTER-FN is a function to
filter the list of retrieved files from the directory. EXCLUDE-DIRECTORIES, if
non-nil, will remove any directories from the list. If SHOW-HIDDEN is non-nil,
will include any files that begin with ."
  (let* ((dir (concat (string-trim-right dir (rx (one-or-more "/"))) "/"))
         (filter (rx line-start (not ".") (zero-or-more not-newline) eol)) ; ^[^.].*$
         (files (directory-files dir nil (if show-hidden nil filter)))
         (filtered-files (if filter-fn (seq-filter filter-fn files) files))
         (non-dir-files (if exclude-directories
                            (seq-filter (lambda (file)
                                          (not
                                           (file-directory-p
                                            (concat dir "/" file))))
                                        filtered-files)
                          filtered-files))
         (selection (completing-read (or prompt "Find file: ") non-dir-files))
         (file-name (concat dir selection)))
    (find-file file-name)))

;;;###autoload
(defun self/lookup-open-link-like-object (lookup-fn &rest args)
  "Advice for LOOKUP-FN. Opens a link-like object: a file, URL, etc."
  (let ((identifier (nth 0 args))
        (url-pattern (rx line-start (seq "http" (? "s") "://")))
        (file-path-pattern (rx line-start (group (one-or-more not-newline)) "/" (group (one-or-more (not "/"))) line-end)))
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

(defun self/goto-line-non-interactive (line-number)
  "Helper for going to a line at LINE-NUMBER without invoking `goto-line'."
  (forward-line (- line-number (line-number-at-pos))))

(defun self/goto-col-non-interactive (col-number)
  "Helper for going to a col at COL-NUMBER without invoking `goto-char' or
`move-to-column'."
  (forward-char (- col-number (current-column))))

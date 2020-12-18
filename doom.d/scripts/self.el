;;; ~/.doom.d/scripts/self.el -*- lexical-binding: t; -*-

;; custom scripts for misc personal use

(require 'evil)

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
  ; TODO automatically insert into document
  (interactive "sPage number: ")
  (insert (format "- %s\n" page-number))
  (insert "\t#+begin_quote\n\t")
  (yank)
  (insert "\n\t#+end_quote"))

; thank you xah
(defun self/unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

; thank you doom emacs discord user zzamboni
; https://discordapp.com/channels/406534637242810369/695219268358504458/788524346309214249
(defun self/org-md-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (or (org-element-property :language src-block) "")))
    (format "```%s\n%s```\n"
            lang
            (org-remove-indentation
             (org-export-format-code-default src-block info)))))

(evil-define-operator self/evil-write-temp (beg end &optional bang)
  "Like evil-write, but creates a new temporary file and writes to that."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<r><!>")
  (let ((s (or beg (point-min)))
        (f (or end (point-max)))
        (bufname (buffer-file-name (buffer-base-buffer))))
    (cond
     ((null bufname) (let ((filename (self/mktemp)))
                       (write-file filename)))
     (t (self/write-temp s f)))))

(defun self/write-temp (beg end)
  "Writes BEG and END from current buffer into a temporary file."
  (with-current-buffer (current-buffer)
    (let ((temp-path (self/mktemp)))
      (write-region beg end temp-path))))

(defun self/mktemp (&optional prefix)
  "Calls mktemp and passes PREFIX to -t flag, defaulting to 'emacs'. Returns result of `mktemp`."
  (let ((pre (or prefix "emacs")))
    (s-trim-right (shell-command-to-string (format "mktemp -t %s" pre)))))

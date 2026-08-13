;;; $DOOMDIR/lisp/+bindings.el -*- lexical-binding: t; -*-

;; every keybinding and evil ex command lives here; the commands they invoke
;; are mostly autoloaded from ../autoload/

(map! :mode org-mode
      :leader "c l" nil
      :leader "c l l" #'link-hint-copy-link
      :leader "c l p" #'link-hint-copy-link-at-point
      :leader "m s s" #'org-insert-subheading)
(map! :leader "."     #'consult-buffer)
(map! :leader "f H"   #'self/dired-here)
(map! :nv "g s l"     #'avy-goto-line)
(map! :leader "w w"   #'ace-window)
(map! :leader "s w"   #'ace-swap-window)
(map! :mode Man-mode
      :n "TAB"        #'man-follow)
(map! :leader "TAB c" #'+workspace/cycle)
(map! :leader "n j t" #'org-journal-open-current-journal-file)
(map! :leader "f o f" #'self/find-org-file)
(map! :leader "f o d" #'self/find-org-file-dir)
(map! :leader "f o w" #'self/open-org-workspace)
(map! :leader "f O"   #'self/visit-common-directories)
(map! :leader "f p"   #'self/find-file-in-private-config)
(map! :leader "d"     #'dired)
(map! :leader "TAB i" #'+ibuffer/open-for-current-workspace)
(map! :leader "i k"   #'consult-yank-from-kill-ring)
(map! :leader "b o"   #'self/new-buffer-with-mode)
(map! :leader "TAB p" #'self/projectile-open-project-in-new-workspace)

(map! (:when (featurep :system 'macos)
        "<apps>" #'execute-extended-command))

;; markdown-mode changes to make consistent with org-mode
;; TODO doesn't quite work?
(map! :map markdown-mode-map
      :ni [C-return] #'markdown-insert-list-item)

;; --------------------------- custom evil operators ---------------------------

;; these use evil macros, so they live here (loaded at startup, after evil)
;; rather than in an autoload file

(evil-define-operator self/evil-write-temp (beg end &optional prefix)
  "Like evil-write, but creates a new temporary file and writes to that."
  :motion nil
  :move-point nil
  :type line
  :repeat nil
  (interactive "<r><a>")
  (let ((s (or beg (point-min)))
        (f (or end (point-max)))
        (tmpfile (make-temp-file (or prefix "wtemp"))))
    (if (buffer-file-name (buffer-base-buffer))
        (write-region s f tmpfile)
      (write-file tmpfile))))

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

;; custom ex commands for evil
(evil-ex-define-cmd "wt[emp]" #'self/evil-write-temp)
(evil-ex-define-cmd "ws[uspend]" #'self/evil-write-suspend)
(evil-ex-define-cmd "shuf" #'self/evil-ex-shuffle-lines)
(evil-ex-define-cmd "uniq" #'self/evil-ex-remove-duplicates)

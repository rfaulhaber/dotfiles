;;; $DOOMDIR/lisp/+tools.el -*- lexical-binding: t; -*-

;; I can never remember the envrc functions
(defalias 'direnv-allow 'envrc-allow)
(defalias 'direnv-reload 'envrc-reload)

;; common directories, used by `self/visit-common-directories'
(setq self/common-directories '(("Downloads" . "~/Downloads")
                                ("Projects" . "~/Projects")
                                ("Screenshots" . "~/Pictures/screenshots")))

;; editor: if line is entirely whitespace when backspace is hit, delete whole line
(setq backward-delete-char-untabify-method 'all)

;; Tramp shell prompt, to allow it to work with terminal colors
;; thank you stackoverflow
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; wdired
;; allow me to edit permissions in wdired
(setq wdired-allow-to-change-permissions t)

;; sops-mode

;; enable global-sops-mode
(global-sops-mode 1)

;; nov
(defun self/nov-setup ()
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (face-remap-add-relative 'variable-pitch :family "Lato" :height 1.5))

(add-hook 'nov-mode-hook #'visual-fill-column-mode)
(add-hook 'nov-mode-hook #'self/nov-setup)
(add-hook 'nov-mode-hook #'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; projectile
(after! projectile
  (setq projectile-switch-project-action 'projectile-dired)
  ;; for some reason projectile can't always find fd
  (let ((fd-exec (executable-find "fd")))
    (when (not projectile-fd-executable)
      (setq projectile-fd-executable fd-exec))

    (when (null doom-fd-executable)
      (setq doom-fd-executable fd-exec))))

;; emacs-everywhere
;; avoids https://github.com/tecosaur/emacs-everywhere/issues/49
(after! emacs-everywhere
  (setq emacs-everywhere-mode-initial-map nil))

;; lookup/documentation advice
(advice-add '+lookup/documentation :around #'self/lookup-open-link-like-object)

;; magit
;; set default clone directory. This is the same on all machines
(setq magit-clone-default-directory "~/Projects/")

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Faulhaber"
      user-mail-address "faulhaberryan@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'doom-laserwave)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; emcas config
;; custom variable settings
(setq diary-file "~/org/diary.org")

;; custom scripts
(load! "./scripts/buffer-move.el")

;; custom bindings
(map!
 :leader
 "f t" #'treemacs
 )

;; custom requires
(require 'dap-lldb)
(require 'org-ref)

;; plugin config
;; hooks
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)
                     (setq lsp-rust-server 'rust-analyzer)
          ))

(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'org-trello-mode)

;; after hooks
(after! org
  (setq org-agenda-files "~/org/todo.org")
  (load! "./scripts/org-templates.el")
)

(after! deft
  (setq deft-directory "~/org")
  (setq deft-recursive t)
)

(after! nov
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
)

(after! org-roam
  (setq org-roam-directory "~/org/roam")
)

(after! org-ref
(setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
      org-ref-default-bibliography '("~/org/bibliography/references.bib")
      org-ref-pdf-directory "~/org/bibliography/bibtex-pdfs/")
)

(after! org-journal
  (setq org-journal-dir "~/org/journal")
)

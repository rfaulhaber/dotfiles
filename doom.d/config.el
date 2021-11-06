;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq self/system-name (string-trim-right (system-name) "\.lan"))
(setq self/system-type (pcase system-type
                         ('gnu/linux "linux")
                         ('darwin "darwin")))

(message "loading configuration for %s on system %s"
         self/system-name
         self/system-type)

;; load machine-specific and type-specific configs
(load! (format "./hosts/%s" self/system-name) nil t)
(load! (format "./hosts/%s" self/system-type) nil t)

;; load custom code
(load! "./scripts/self.el")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

(setq user-full-name "Ryan Faulhaber"
      user-mail-address "ryf@sent.as"
      calendar-latitude 41.49
      calendar-longitude 81.69
      calendar-location-name "Cleveland, OH")

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
(setq doom-font (font-spec :family "Hack Nerd Font" :size config/font-size))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-city-lights)

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

;; custom bindings
(map! :leader :desc "Opens EWW url to the right" "o w" #'self/eww-open-url-window-right )
(map! :leader "e"     #'elfeed)
(map! :mode org-mode
      :leader "c l" nil
      :leader "c l l" #'link-hint-copy-link
      :leader "c l p" #'link-hint-copy-link-at-point
      :leader "m s s" #'org-insert-subheading)
(map! :leader "."     #'+ivy/switch-buffer)
(map! :leader "f H"   #'self/dired-here)
(map! :nv "g s l"     #'avy-goto-line)
(map! :leader "w w"   #'ace-window)
(map! :leader "s w"   #'ace-swap-window)
(map! :leader "n b"   #'nix-buffer)
(map! :leader "f b"   #'calibredb-find-counsel)
(map! :leader "s R"   #'counsel-evil-marks)
(map! :mode Man-mode
      :n "TAB"        #'man-follow)
(map! :leader "TAB c" #'+workspace/cycle)
(map! :leader "n j t" #'org-journal-open-current-journal-file)
(map! :leader "f o"   #'self/find-org-file)
(map! :leader "f O"   #'self/visit-common-directories)
(map! :leader "d"     #'dired)
(map! :leader "TAB i" #'+ibuffer/open-for-current-workspace)

;; common directories
(setq self/common-directories '(("Downloads" . "~/Downloads")
                                ("Projects" . "~/Projects")
                                ("Screenshots" . "~/Pictures/screenshots")))

;; editor: if line is entirely whitespace when backspace is hit, delete whole line
(setq backward-delete-char-untabify-method 'all)

;; custom ex commands for evil
(evil-ex-define-cmd "wt[emp]" #'self/evil-write-temp)
(evil-ex-define-cmd "ws[uspend]" #'self/evil-write-suspend)

;; custom variable settings
;; Tramp shell prompt, to allow it to work with terminal colors
;; thank you stackoverflow
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; wdired
;; allow me to edit permissions in wdired
(setq wdired-allow-to-change-permissions t)

;; GPG key used by org-crypt
(setq org-crypt-key "A2205925F3B6C5B96F26C3CB544650C5A306061B")

;; use rust-analyzer for rust lsp server
(setq rustic-lsp-server 'rust-analyzer)

;; org
(after! org
  (load! "./scripts/org-templates.el"))

;; org-md
;; the default md exporter for source code blocks is bad, so we replace it
(advice-add 'org-md-example-block :override #'self/org-md-src-block)

;; the default md exporter should always unfold paragraphs
(advice-add 'org-md-paragraph :filter-args #'self/org-md-paragraph-unfill)

;; org-agenda
(setq org-agenda-files
      (mapcar
       (lambda (str)
         (concat org-directory "/" str))
       (list "todo.org" "habits.org" "projects.org" "blog.org")))

;; deft
(setq deft-directory "~/org")
(setq deft-recursive t)

;; org-roam
(setq org-roam-directory "~/org/roam")
(setq org-roam-graph-exclude-matcher '("daily"))

;; for adding backlinks to exported org-roam files
(add-hook 'org-export-before-processing-hook #'self/org-export-preprocessor)


;; org-roam-server-mode
(after! org
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; org-journal
(setq
 org-journal-dir "~/org/journal"
 org-journal-file-format "%Y%m%d.org")

;; org-ref
(setq bibtex-completion-notes-path "~/org/bibliography/notes.org"
      bibtex-completion-bibliography '("~/org/bibliography/references.bib"))

;; org-publish
;; TODO add hooks for publishing roam files
(setq org-publish-project-alist '(("roam"
                                   :base-directory "~/org/roam"
                                   :base-extension "org"
                                   :publishing-directory "~/Projects/roam-notes-site"
                                   :publishing-function org-html-publish-to-html
                                   :with-author nil
                                   :recursive t)
                                  ("agora"
                                   :base-directory "~/org/roam"
                                   :base-extension "org"
                                   :publishing-directory "~/Projects/roam"
                                   :publishing-function org-agora-publish-to-agora
                                   :recursive t)))

;; nov
(defun nov-setup ()
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (face-remap-add-relative 'variable-pitch :family "Lato" :height 1.5))

(add-hook 'nov-mode-hook 'nov-setup)
(add-hook 'nov-mode-hook 'visual-line-mode)

;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; calibredb
(setq calibredb-root-dir "~/calibre")
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))

;; mu4e
(setq
 mail-user-agent 'mu4e-user-agent
 mu4e-sent-messages-behavior 'sent
 mu4e-main-mode-hook (lambda ()
                       (setq mu4e-sent-messages-behavior 'sent))
 mu4e-sent-folder   "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash"
 mu4e-refile-folder "/Archive"
 smtpmail-default-smtp-server "smtp.fastmail.com"
 smtpmail-smtp-server "smtp.fastmail.com"
 smtpmail-smtp-user "ryf@sent.as"
 smtpmail-local-domain "sent.as"
 smtpmail-smtp-service 587)

(after! mu4e
  (add-to-list 'mu4e-bookmarks
               '(:name "Inbox"
                 :query "maildir:/Inbox"
                 :key ?i)))


;; elfeed
(setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org")))

;; wttrin
(setq wttrin-default-cities '("Cleveland"))

;; replace wttrin-fetch-raw-string with my own function
(advice-add 'wttrin-fetch-raw-string :override 'self/wttrin-fetch-raw-string)

;; gnus
(setq gnus-select-method '(nntp "us.newsdemon.com"))

;; ix
(setq
 ix-user "sys9"
 ix-token (string-trim (shell-command-to-string "pass ix")))

;; eshell
(add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))

;; projectile
(setq projectile-switch-project-action 'projectile-dired)

;; counsel-projectile
;; this changes the behavior of `counsel-projectile-switch-project' to open the
;; root of the project in Dired. most of the time this is what I want. see the
;; documentation for this variable for more info. we also have to load this
;; variable after `counsel-projectile' loads, because `counsel-projectile'
;; defines this variable.
(after! counsel-projectile
  (setcar counsel-projectile-switch-project-action 5))

;; hooks
(add-hook 'nov-mode-hook 'nov-setup)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)
(add-hook 'before-save-hook #'+format/buffer)

(after! undo-tree
  (setq undo-tree-auto-save-history nil))

;; load languages for org-babel
;; these are mostly for use in reveal
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (typescript . t)
   (js . t)))


;; see: https://github.com/hlissner/doom-emacs/issues/3185
(defadvice! self/+org-inline-image-data-fn (_protocol link _description)
  :override #'+org-inline-image-data-fn
  "Interpret LINK as base64-encoded image data. Ignore all errors."
  (with-demoted-errors "%S" (base64-decode-string link)))

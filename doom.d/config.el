;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


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

(setq self/font-size (if (string= (system-name) "mir3") 24 14))
(setq doom-font (font-spec :family "Hack Nerd Font" :size self/font-size))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-moonlight)
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

;; custom scripts
(load! "./scripts/buffer-move.el")
(load! "./scripts/self.el")

;; custom bindings
(map! :leader "f t" #'treemacs)
(map! :leader "n r t" #'org-roam-buffer-toggle-display)
(map! :leader "o w" #'self/eww-open-url-window-right)
(map! :leader "e" #'elfeed)
(map! :mode org-mode
      :leader "c l" nil
      :leader "c l l" #'link-hint-copy-link
      :leader "c l p" #'link-hint-copy-link-at-point)
(map! :leader "." #'+ivy/switch-buffer)
(map! :leader "d" #'dired)
(map! :nv "g s l" #'avy-goto-line)
(map! :leader "w w" #'ace-window)
(map! :leader "s w" #'ace-swap-window)
(map! :leader "n b" #'nix-buffer)
(map! :leader "f b" #'calibredb-find-counsel)
(map! :leader "s R" #'counsel-evil-marks)
;; TODO get this to work
;; (map! :mode calibredb-search-mode
;;       "RET" #'calibredb-find-file)

;; TODO get these to work!
;; (map! :mode cfw:details-mode
;;       "q" #'cfw:details-kill-buffer-command)
;; (map! :mode cfw:calendar-mode
;;       "RET" #'cfw:show-details-command)

;; custom ex commands for evil
(evil-ex-define-cmd "wt[emp]" #'self/evil-write-temp)

;; custom variable settings
;; Tramp shell prompt, to allow it to work with terminal colors
;; thank you stackoverflow
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; GPG key used by org-crypt
(setq org-crypt-key "A2205925F3B6C5B96F26C3CB544650C5A306061B")

;; use rust-analyzer for rust lsp server
(setq rustic-lsp-server 'rust-analyzer)

;; set aspell program specifically on mac
(when (string= system-type "darwin")
  (setq ispell-program-name "/usr/local/bin/aspell"))

;; org-md
;; the default md exporter for source code blocks is bad, so we replace it
(advice-add 'org-md-example-block :override 'self/org-md-src-block)

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
;; (setq org-roam-graph-exclude-matcher '("literature" "daily"))
(setq org-roam-graph-exclude-matcher '("daily"))

;; personal helper variables
(setq self/org-roam-default-file-name-template "%<%Y%m%d%H%M%S>-${slug}")
(setq self/org-roam-default-file-head-template "#+title: ${title}\n")

;; TODO change to add-to-list
(setq org-roam-capture-templates
      `(("d" "default" plain #'org-roam-capture--get-point
     "%?"
     :file-name ,self/org-roam-default-file-name-template
     :head ,self/org-roam-default-file-head-template
     :unnarrowed t)
        ("p" "permanent" plain #'org-roam-capture--get-point
     "- tags :: %?
- source ::
- relevant notes:
  +"
     :file-name ,self/org-roam-default-file-name-template
     :head ,self/org-roam-default-file-head-template
     :unnarrowed t)
        ("l" "literature" plain #'org-roam-capture--get-point
     "%?"
     :file-name ,(format "literature/%s" self/org-roam-default-file-name-template)
     :head ,self/org-roam-default-file-head-template
     :unnarrowed t)
        ))

(setq org-roam-dailies-capture-templates
      '(("d" "daily" plain (function org-roam-capture--get-point) ""
    :file-name "daily/%<%Y%m%d>"
    :head "#+title: %<%Y%m%d>")))

;; org-roam-server-mode

(setq
  org-roam-server-host                          "127.0.0.1"
  org-roam-server-port                          8080
  org-roam-server-authenticate                  nil
  org-roam-server-export-inline-images          t
  org-roam-server-serve-files                   nil
  org-roam-server-served-file-extensions        '("pdf" "mp4" "ogv")
  org-roam-server-network-poll                  t
  org-roam-server-network-arrows                nil
  org-roam-server-network-label-truncate        t
  org-roam-server-network-label-truncate-length 60
  org-roam-server-network-label-wrap-length     20)

;; org-journal
(setq
 org-journal-dir "~/org/journal"
 org-journal-file-format "%Y%m%d.org"
 )

;; org-ref
(setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
      org-ref-default-bibliography '("~/org/bibliography/references.bib")
)

;; org-publish
(setq org-publish-project-alist '(
                                  ("roam"
                                  :base-directory "~/org/roam"
                                  :base-extension "org"
                                  :publishing-directory "~/Projects/roam-notes-site"
                                  :publishing-function org-html-publish-to-html
                                  :recursive t)
                                  ))

;; nov config
(defun nov-setup ()
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (face-remap-add-relative 'variable-pitch :family "Lato" :height 1.5))

(add-hook 'nov-mode-hook 'nov-setup)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

;; auto-mode-alist config
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; calibredb config
(setq calibredb-root-dir "~/calibre")
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))

; TODO bind calibredb-find-file

;; mu4e config
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

;; elfeed config
(setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org")))

;; plugin config
;; hooks

(add-hook 'after-init-hook 'org-roam-mode)
;(add-hook 'after-init-hook 'org-trello-mode)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'nov-mode-hook 'nov-setup)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(add-hook 'before-save-hook (lambda ()
                              (when (or (eq 'rust-mode major-mode) (eq 'rustic-mode major-mode))
                                (lsp-format-buffer))))


;; after hooks
(after! org
  (load! "./scripts/org-templates.el")
)

;; load languages for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (typescript . t)
   (js . t)))

;; eshell config
(add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))

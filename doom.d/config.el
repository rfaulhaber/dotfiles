;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defvar config/work-computer-p nil "If t, is a work computer.")
(defvar config/font-size 16 "Font size. Should be specified by a host.")
(defvar self/system-name (string-trim-right (system-name) (rx (or "\.lan" "\.attlocal.net" "\.local")))
  "System name. Used in loading init scripts.")
(defvar self/system-type (pcase system-type
                           ('gnu/linux "linux")
                           ('darwin "darwin"))
  "System type. Either 'linux' or 'darwin'.
Used in loading config specific to those systems.")

(message "loading configuration for %s on system %s"
         self/system-name
         self/system-type)

;; load machine-specific and type-specific configs
(load! (format "./hosts/%s" self/system-name) nil t)
(load! (format "./hosts/%s" self/system-type) nil t)

;; load custom code
(load! "./self/self.el")

;; if a work computer, load additional config
(when config/work-computer-p
  (load! "./self/work.el")
  (load! "./self/work-journal.el"))

;; --------------------------------- doom basics ----------------------------------

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

(setq user-full-name "Ryan Faulhaber"
      user-mail-address "ryf@sent.as"
      calendar-latitude 41.49
      calendar-longitude -81.69
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
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size config/font-size))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)

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

;; -------------------------------- emacs basics --------------------------------

;; custom bindings
(map! :leader :desc "Opens EWW url to the right" "o w" #'self/eww-open-url-window-right )
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
(map! :leader "f b"   #'calibredb-find-counsel)
(map! :mode Man-mode
      :n "TAB"        #'man-follow)
(map! :leader "TAB c" #'+workspace/cycle)
(map! :leader "n j t" #'org-journal-open-current-journal-file)
(map! :leader "f o f"   #'self/find-org-file)
(map! :leader "f o d"   #'self/find-org-file-dir)
(map! :leader "f o w" #'self/open-org-workspace)
(map! :leader "f O"   #'self/visit-common-directories)
(map! :leader "d"     #'dired)
(map! :leader "TAB i" #'+ibuffer/open-for-current-workspace)
;; racket-mode doesn't define evil-jump-item and it's annoying
(map! :mode racket-mode
      "TAB" #'evil-jump-item)

(map! :leader "b o" #'self/new-buffer-with-mode)
(map! :leader "TAB p" #'self/projectile-open-project-in-new-workspace)

(map! (:when (featurep :system 'macos)
        "<apps>" #'execute-extended-command))

;; markdown-mode changes to make consistent with org-mode
(map! :map markdown-mode-map
      :ni [C-return] #'markdown-insert-list-item)

;; common directories
(setq self/common-directories '(("Downloads" . "~/Downloads")
                                ("Projects" . "~/Projects")
                                ("Screenshots" . "~/Pictures/screenshots")))

;; editor: if line is entirely whitespace when backspace is hit, delete whole line
(setq backward-delete-char-untabify-method 'all)

;; custom ex commands for evil
(evil-ex-define-cmd "wt[emp]" #'self/evil-write-temp)
(evil-ex-define-cmd "ws[uspend]" #'self/evil-write-suspend)
(evil-ex-define-cmd "shuf" #'self/evil-ex-shuffle-lines)
(evil-ex-define-cmd "uniq" #'self/evil-ex-remove-duplicates)

;; custom variable settings
;; Tramp shell prompt, to allow it to work with terminal colors
;; thank you stackoverflow
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

;; wdired
;; allow me to edit permissions in wdired
(setq wdired-allow-to-change-permissions t)

;; GPG key used by org-crypt
(setq org-crypt-key "A90BC7B722983F6BB8EAC1DA144A6B5FBB68FC9D")

;; rust/rustic

;; use rust-analyzer for rust lsp server
(setq rustic-lsp-server 'rust-analyzer)

;; use treesit instead of emacs-tree-sitter for rust
(setq rust-mode-treesitter-derive t)

;; --------------------------------- org mode ---------------------------------
(after! org
  (load! "./self/org-templates.el")
  (org-crypt-use-before-save-magic))

;; for org mode, set the fill column to 130
(add-hook 'org-mode-hook (lambda ()
                           (setq-local fill-column 130)))

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
       (append
        (list
         "todo.org"
         "habits.org"
         "projects.org"
         "blog.org"
         "todo")
        (when config/work-computer-p
          (list (format-time-string "journal/%Y%m%d.org"))))))

;; org-roam
(setq
 org-roam-directory "~/org/roam"
 org-roam-graph-exclude-matcher '("daily"))

;; for adding backlinks to exported org-roam files
(add-hook 'org-export-before-processing-functions #'self/org-roam-export-refs)

;; org-roam-server-mode
(after! org
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)

  ;; org-modern
  (setq org-modern-list
        '((?* . "•")
          (?+ . "‣"))
        org-modern-fold-stars
        '(("◉" . "○")
          ("◉" . "○")
          ("▸" . "▾")
          ("▸" . "▾"))
        org-modern-checkbox
        '((?X . "󱗼")
          (?- . "┅")
          (?\s . " "))))

;; org-journal
(setq
 org-journal-dir "~/org/journal"
 org-journal-file-format "%Y%m%d.org")

(add-hook 'org-journal-mode-hook (lambda ()
                                   (setq org-element-use-cache nil)))

(when config/work-computer-p
  (setq org-journal-file-header #'work/org-journal-file-header)
  (add-hook 'org-journal-after-header-create-hook #'work/org-journal-after-header-create-hook))

;; org-ref
(setq bibtex-completion-notes-path "~/org/bibliography/notes.org"
      bibtex-completion-bibliography '("~/org/bibliography/references.bib"))

;; ox-agora
;; thank you again Neil
(advice-add 'org-export-output-file-name :filter-return #'self/slugify-export-output-file-name)

;; sometimes org publish complains about not being able to resolve ids. This is
;; a workaround for that
(advice-add 'org-publish :before #'self/org-publish-before-advice)

;; org-publish
;; TODO add hooks for publishing roam files
(setq org-publish-project-alist '(("roam web"
                                   :base-directory "~/org/roam"
                                   :base-extension "org"
                                   :publishing-directory "~/Projects/roam-web"
                                   :publishing-function org-html-publish-to-html
                                   :with-author nil
                                   :recursive t)
                                  ("agora"
                                   :base-directory "~/org/roam"
                                   :base-extension "org"
                                   :publishing-directory "~/Projects/roam"
                                   :publishing-function org-agora-publish-to-agora
                                   :recursive t
                                   :headline-levels 4
                                   :with-toc nil)))

;; ------------------------------- end org mode -------------------------------

;; nov
(defun nov-setup ()
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (face-remap-add-relative 'variable-pitch :family "Lato" :height 1.5))

(add-hook 'nov-mode-hook 'visual-fill-column-mode)
(add-hook 'nov-mode-hook 'nov-setup)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; calibredb
(setq calibredb-root-dir "~/calibre")
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))

;; mu4e
(setq
 mail-user-agent 'mu4e-user-agent
 mu4e-sent-messages-behavior 'sent
 mu4e-main-mode-hook (function
                      (lambda ()
                        (setq mu4e-sent-messages-behavior 'sent)))
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


;; wttrin
(setq wttrin-default-cities '("Cleveland"))

;; replace wttrin-fetch-raw-string with my own function
(advice-add 'wttrin-fetch-raw-string :override #'self/wttrin-fetch-raw-string)

;; gnus
(setq gnus-select-method '(nntp "us.newsdemon.com"))

;; eshell
(add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))

;; projectile
(after! projectile
  (setq projectile-switch-project-action 'projectile-dired)
  ;; for some reason projectile can't always find fd
  (let ((fd-exec (executable-find "fd")))
    (when (not projectile-fd-executable)
      (setq projectile-fd-executable fd-exec))

    (when (null doom-fd-executable)
      (setq doom-fd-executable fd-exec))))

;; lisp mode
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

;; emacs-everywhere
;; avoids https://github.com/tecosaur/emacs-everywhere/issues/49
(after! emacs-everywhere
  (setq emacs-everywhere-mode-initial-map nil))

;; lookup/documentation advice
(advice-add '+lookup/documentation :around #'self/lookup-open-link-like-object)

;; magit
;; set default clone directory. This is the same on all machines
(setq magit-clone-default-directory "~/Projects/")

;; nix mode
;; set formatter to alejandra
(set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))

;; apheleia
(after! apheleia
  ;; add formatter for alejandra
  (push '(alejandra . ("alejandra" "-")) apheleia-formatters)

  ;; set nix to use alejandra rather than nixfmt
  (setf (alist-get 'nix apheleia-mode-alist) 'alejandra))

;; configure nix-ts-mode
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
(add-hook 'nix-ts-mode-hook 'eglot-ensure)

;; lua mode
(after! lua-mode
  ;; set LSP location
  (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server")))

;; nushell-ts-mode
(if (treesit-language-available-p 'nu)
    (add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-ts-mode))
  (message "treesit language unavailable for nu!"))

;; eglot
(after! eglot
  (add-to-list 'eglot-server-programs
               '((nix-ts-mode nix-mode) . ("nil" :initializationOptions
                                           (:formatting (:command ["alejandra" "--quiet" "-"])))))

  (add-hook 'nix-ts-mode-hook #'eglot-ensure)
  (add-hook 'nix-mode-hook #'eglot-ensure)

  (add-to-list 'eglot-server-programs
               '(nushell-ts-mode . ("nu" "--lsp")))

  (add-hook 'nushell-ts-mode-hook #'eglot-ensure)

  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode elixir-mode) . ("elixir-ls")))

  (add-hook 'elixir-mode-hook #'eglot-ensure)
  (add-hook 'elixir-ts-mode-hook #'eglot-ensure)

  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("tflint" "--langserver"))))

;; kdl mode
(add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode))



;; ---------------------------------- treesit ----------------------------------

(setq treesit-language-source-alist '((nu . ("https://github.com/nushell/tree-sitter-nu" "main"))
                                      (nix . ("https://github.com/nix-community/tree-sitter-nix"))))

;; ----------------------------------- misc ------------------------------------

;; general
;; commented out for now, `'+format/buffer''s behavior has changed
;; (add-hook 'before-save-hook #'+format/buffer)

;; dynamically load languages for org-babel
;; thank you r/emacs: https://www.reddit.com/r/emacs/comments/us7zae/comment/i9ceaco
(advice-add 'org-babel-execute-src-block :around #'self/org-babel-execute-src-block-lazy-load)

;; see: https://github.com/hlissner/doom-emacs/issues/3185
(advice-add '+org-inline-image-data-fn :override #'self/+org-inline-image-data-fn)

;; --------------------------------- quickrun ---------------------------------

;; adds custom nushell runner
(quickrun-add-command "nushell"
  '((:command . "nu"))
  :default "nushell"
  :mode 'nushell-ts-mode)

;; TODO implement error handling
;; (quickrun-add-command "fennel"
;;   '((:command . "fennel")
;;     (:exec    (lambda ()
;;                 (let* ((tmpfile (shell-command-to-string "mktemp -t quickrun-fennel.XXXX"))
;;                        (save-cmd (concat "save -f " tmpfile)))
;;                   `(,(concat "%c --compile %s | " save-cmd)
;;                     ,(format "luajit %s" tmpfile))))))
;;   :default "fennel"
;;   :mode 'fennel-mode)

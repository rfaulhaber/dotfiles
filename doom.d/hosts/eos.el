;;; ../Projects/dotfiles/doom.d/hosts/eos.el -*- lexical-binding: t; -*-

(message "loading eos config")

(set-frame-size (selected-frame) 145 46)

;; Ensure nix paths are in exec-path and PATH
;; IMPORTANT: Prepend to existing PATH rather than replacing it entirely,
;; otherwise envrc/direnv won't work correctly (it merges with default process-environment)
(let ((path-elements '("/run/current-system/sw/bin"
                       "/etc/profiles/per-user/ryan/bin"
                       "/usr/bin"
                       "/usr/local/bin")))
  (cl-loop for el in path-elements
           do
           (add-to-list 'exec-path el))

  ;; Prepend nix paths to existing PATH, preserving the rest
  (setenv "PATH" (string-join (delete-dups
                               (append path-elements
                                       (parse-colon-path (getenv "PATH"))))
                              ":")))

(setq doom-ripgrep-executable "/etc/profiles/per-user/ryan/bin/rg"
      insert-directory-program "/etc/profiles/per-user/ryan/bin/ls"
      dired-listing-switches "-ahl --group-directories-first"
      ;; emacs seems to assume nushell is installed by homebrew
      shell-file-name "/run/current-system/sw/bin/nu"
      sh-shell-file "/run/current-system/sw/bin/nu")

(after! dired
  ;; this effectively cancels out the doom hook located at
  ;; modules/emacs/dired/config.el:41-51
  ;; the problem is that doom assumes that if it can't find gls you must be using
  ;; BSD ls necessarily, which I'm not :)
  (let ((args (list "-avhl" "--group-directories-first")))
    (setq dired-listing-switches (string-join args " "))
    (setq dired-actual-switches args)))

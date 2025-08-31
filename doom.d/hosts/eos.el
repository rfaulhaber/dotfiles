;;; ../Projects/dotfiles/doom.d/hosts/eos.el -*- lexical-binding: t; -*-

(set-frame-size (selected-frame) 145 46)

;; not sure why the PATH isn't loading correctly, so I've opted to manually fix
;; it here
(let ((path-elements '("/run/current-system/sw/bin"
                       "/etc/profiles/per-user/ryan/bin"
                       "/usr/bin"
                       "/usr/local/bin")))
  (cl-loop for el in path-elements
           do
           (add-to-list 'exec-path el))

  (setenv "PATH" (string-join path-elements ":")))

(setq doom-ripgrep-executable "/etc/profiles/per-user/ryan/bin/rg"
      insert-directory-program "/etc/profiles/per-user/ryan/bin/ls"
      dired-listing-switches "-ahl --group-directories-first")

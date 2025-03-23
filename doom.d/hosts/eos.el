;;; ../Projects/dotfiles/doom.d/hosts/eos.el -*- lexical-binding: t; -*-

(set-frame-size (selected-frame) 145 46)

;; not sure why the PATH isn't loading correctly, so I've opted to manually fix
;; it here
(let ((path-elements '("/Users/ryan/Library/Application Support/carapace/bin"
                       "/Users/ryan/.nix-profile/bin"
                       "/opt/homebrew/bin"
                       "/Users/ryan/.emacs.d/bin"
                       "/Users/ryan/.config/emacs/bin"
                       "/usr/bin"
                       "/bin"
                       "/usr/sbin"
                       "/sbin"
                       "/usr/local/bin"
                       "/nix/var/nix/profiles/default/bin")))
  (cl-loop for el in path-elements
           do
           (add-to-list 'exec-path el))

  (setenv "PATH" (string-join path-elements ":")))

;; for some reason projectile can't find fd
(let ((fd-exec (executable-find "fd")))
  (when (not projectile-fd-executable)
    (setq projectile-fd-executable fd-exec))

  (when (null doom-fd-executable)
    (setq doom-fd-executable fd-exec)))

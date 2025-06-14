;;; ../Projects/dotfiles/doom.d/hosts/darwin.el -*- lexical-binding: t; -*-

;; explicitly set `ispell-program-name'
;; (setq ispell-program-name "/usr/local/bin/aspell")

;; do some key remapping
(setq mac-option-buffer          'meta
      mac-right-option-modifier  'meta
      mac-command-modifier       'super
      mac-right-command-modifier 'super)

;; browse-url needs some help on macOS
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program  "open")

(setq doom-ripgrep-executable "/opt/homebrew/bin/rg")

;; temporary --- reset woman-manpath to original value
(setq woman-manpath '("/opt/homebrew/share/man/" "/usr/share/man/" "/usr/local/share/man/" "/Applications/kitty.app/Contents/Resources/man/" "/usr/share/man" "/usr/local/share/man" "/usr/X11/man" "/Library/Apple/usr/share/man"))

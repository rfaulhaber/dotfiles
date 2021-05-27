;;; ../Projects/dotfiles/doom.d/hosts/darwin.el -*- lexical-binding: t; -*-

;; explicitly set `ispell-program-name'
(setq ispell-program-name "/usr/local/bin/aspell")

;; do some key remapping
(setq mac-option-buffer 'meta
      mac-right-option-modifier 'meta
      mac-command-modifier 'super
      mac-right-command-modifier 'super)

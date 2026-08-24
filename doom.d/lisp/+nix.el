;;; $DOOMDIR/lisp/+nix.el -*- lexical-binding: t; -*-

;; .nix files prefer the treesit mode and fall back to nix-mode without the
;; grammar; formatting comes from nil/alejandra via eglot (the :editor format
;; module's +lsp flag prefers the LSP formatter in eglot-managed buffers)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . self/nix-ts-mode-maybe))

;; `eglot-ensure' is autoloaded, so the mode hooks must live at top level: a
;; hook registered inside `after! eglot' would only take effect once something
;; else had already loaded eglot.
(add-hook 'nix-ts-mode-hook #'eglot-ensure)
(add-hook 'nix-mode-hook #'eglot-ensure)

(after! eglot
  (add-to-list 'eglot-server-programs
               '((nix-ts-mode nix-mode) . ("nil" :initializationOptions
                                           (:formatting (:command ["alejandra" "--quiet" "-"]))))))

;; `treesit-language-source-alist' is a plain defvar in treesit.el with no
;; autoload cookie, so it stays void until something actually pulls treesit in.
;; Nothing guarantees that has happened by the time this file loads.
(after! treesit
  (add-to-list 'treesit-language-source-alist
               '(nix . ("https://github.com/nix-community/tree-sitter-nix"))))

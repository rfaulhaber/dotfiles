;;; $DOOMDIR/lisp/+langs.el -*- lexical-binding: t; -*-

;; language setup that doesn't warrant its own file; nix lives in +nix.el

;; rust/rustic

;; use rust-analyzer for rust lsp server
(setq rustic-lsp-server 'rust-analyzer)

;; use treesit instead of emacs-tree-sitter for rust
(setq rust-mode-treesitter-derive t)

;; lua mode
(after! lua-mode
  ;; set LSP location
  (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server")))

;; nushell-ts-mode
(add-to-list 'auto-mode-alist '("\\.nu\\'" . self/nushell-ts-mode-maybe))

;; deferred because treesit may not be loaded yet (see the note in +nix.el)
(after! treesit
  (add-to-list 'treesit-language-source-alist
               '(nu . ("https://github.com/nushell/tree-sitter-nu" "main"))))

;; kdl mode
(add-to-list 'auto-mode-alist '("\\.kdl\\'" . self/kdl-ts-mode-maybe))

;; lisp mode
(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)

;; eglot: `eglot-ensure' hooks live at top level (see the note in +nix.el)
(add-hook 'nushell-ts-mode-hook #'eglot-ensure)
(add-hook 'elixir-mode-hook #'eglot-ensure)
(add-hook 'elixir-ts-mode-hook #'eglot-ensure)

(after! eglot
  (add-to-list 'eglot-server-programs
               '(nushell-ts-mode . ("nu" "--lsp")))
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode elixir-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("tflint" "--langserver"))))

;; quickrun: custom nushell runner
(quickrun-add-command "nushell"
  '((:command . "nu"))
  :default "nushell"
  :mode 'nushell-ts-mode)

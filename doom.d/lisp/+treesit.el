;;; $DOOMDIR/lisp/+treesit.el -*- lexical-binding: t; -*-

;; Grammar availability is a property of the machine, not of this repo, which is
;; shared with Emacs installations Nix doesn't manage. Two things can be missing
;; independently: tree-sitter support in the build, which leaves
;; `treesit-language-available-p' void and makes merely loading nix-ts-mode
;; signal an error, and the grammar for an individual language.
;;
;; Deciding at file-open time rather than at init also means a newly installed
;; grammar takes effect without restarting Emacs: nothing caches a negative
;; result, so `treesit-language-available-p' re-checks the filesystem per call.

(defun self/treesit-mode-or-fallback (lang ts-mode &optional fallback)
  "Enter TS-MODE if LANG's tree-sitter grammar is installed, else FALLBACK.
Without a usable FALLBACK the buffer is left in `fundamental-mode'.  That
is deliberate: a ts-mode whose grammar is missing quietly skips its own
font-lock and indentation setup, so it looks like a working mode while
behaving like a broken one."
  (cond ((and (fboundp 'treesit-language-available-p)
              (treesit-language-available-p lang)
              (fboundp ts-mode))
         (funcall ts-mode))
        ((and fallback (fboundp fallback))
         (funcall fallback))
        (t
         (message "no tree-sitter grammar for `%s'; leaving %s alone"
                  lang (buffer-name)))))

(defun self/nix-ts-mode-maybe ()
  "Enter `nix-ts-mode', or `nix-mode' when the nix grammar is missing."
  (interactive)
  (self/treesit-mode-or-fallback 'nix #'nix-ts-mode #'nix-mode))

(defun self/nushell-ts-mode-maybe ()
  "Enter `nushell-ts-mode' when the nu grammar is installed."
  (interactive)
  (self/treesit-mode-or-fallback 'nu #'nushell-ts-mode))

(defun self/kdl-ts-mode-maybe ()
  "Enter `kdl-ts-mode' when the kdl grammar is installed."
  (interactive)
  (self/treesit-mode-or-fallback 'kdl #'kdl-ts-mode))

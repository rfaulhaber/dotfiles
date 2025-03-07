;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's
;;      modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input

       :completion
       ;; (corfu +orderless)
       company           ; the ultimate code completion backend
       vertico           ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing
       (emoji +unicode)

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave +lsp)  ; automated prettiness
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired +dirvish +icons)            ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +flymake +icons)              ; tasing you for every semicolon you forget
       (spell +flyspell +aspell +everywhere)            ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +dictionary +offline +docsets)              ; navigate your code and its documentation
       (lsp +eglot +peek)
       magit             ; a git porcelain for Emacs
       pdf               ; pdf enhancements

       :lang
       common-lisp       ; if you've seen one lisp, you've seen them all
       data              ; config/data formats
       (elixir +lsp)           ; erlang done right
       emacs-lisp        ; drown in parentheses
       (json +lsp)              ; At least it ain't XML
       (javascript +lsp)          ; all(hope(abandon(ye(who(enter(here))))))
       latex             ; writing papers in Emacs has never been so fun
       markdown            ; writing docs for people to ignore
       (nix +lsp)              ; I hereby declare "nix geht mehr!"
       (org +hugo +journal +pandoc +present +roam2 +noter +dragndrop +pretty +pomodoro)               ; organize your plain life in plain text
       (python +lsp +pyright)           ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       (rust +lsp)               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scheme            ; a fully conniving family of lisps
       (sh +lsp)         ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)               ; the tubes
       yaml              ; JSON, but readable

       :email
       ;; (mu4e +gmail)

       :app
       everywhere
       calendar

       :os
       (:if IS-MAC macos)             ; MacOS-specific commands

       :config
       ;;literate
       (default +bindings +smartparens))

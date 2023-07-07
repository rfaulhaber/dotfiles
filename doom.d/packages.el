;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;;(package! another-package
;; :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;; :recipe (:host github :repo "username/repo"
;;          :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; unpinned packages
(unpin! org-roam)

;; *elpa packages
(package! calibredb)
(package! deadgrep)
(package! mermaid-mode)
(package! nov)
(package! ob-mermaid)
(package! ob-typescript)
(package! org-cliplink)
(package! pollen-mode)
(package! prettier)
(package! fzf)
;; (package! hyperbole)

;; non-*elpa packages
(package! caseconv
  :recipe (:host github
           :repo "rfaulhaber/caseconv.el"
           :build t))

(package! nix-local-buffer
  :recipe (:host github
           :repo "rfaulhaber/nix-local-buffer"
           :build t))

(package! ox-agora
  :recipe (:host gitlab
           :repo "ngm/ox-agora"
           :build t))

(package! screenshot
  :recipe (:host github
           :repo "tecosaur/screenshot"
           :branch "master"))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! zoxide.el
  :recipe (:host gitlab
           :repo "Vonfry/zoxide.el"
           :branch "master"))

(package! nushell-mode
  :recipe (:host github
           :repo "mrkkrp/nushell-mode"))

;; org roam ui
(package! websocket)
(package! org-roam-ui
  :recipe (:host github
           :repo "org-roam/org-roam-ui"
           :build t
           :files ("*.el" "out")))

;; this is to get around the fact that the package version of this file is
;; hosted on a server that sometimes goes down
(package! json-process-client
  :recipe (:host github
           :repo "emacsmirror/json-process-client"))

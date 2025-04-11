;; -*- no-byte-compile: t; lexical-binding: t; -*-
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

;; NOTE many of these packages that pull from github repos are pinned to satisfy
;; the requirements of nix-doom-emacs-unstraightened

;; unpinned packages
;; (unpin! org-roam)

;; *elpa packages
(package! deadgrep)
(package! mermaid-mode)
(package! nov)
(package! ob-mermaid)
(package! ob-typescript)
(package! org-cliplink)
(package! pollen-mode)
(package! prettier)
(package! kurecolor)
(package! nushell-ts-mode)
(package! realgud-lldb)
(package! nix-ts-mode)

;; (package! hyperbole)

;; rustic isn't maintained anymore and doom still uses the main branch
(package! rustic
  :pin "22a5ef8bfd5a34ced945c2722938eb29632371d4"
  :recipe (:host github
           :repo "emacs-rustic/rustic"))

;; non-*elpa packages
(package! caseconv 
  :pin "b5395df6a734b70acf5cc76b9d64039ce5305009"
  :recipe (:host github
           :repo "rfaulhaber/caseconv.el"))

(package! nix-local-buffer
  :pin "3cfc659c1e3ff95e03321462a3e12e04622174f2"
  :recipe (:host github
           :repo "rfaulhaber/nix-local-buffer"))

(package! screenshot
  :pin "2770c0cfefe1cc09d55585f4f2f336a1b26e610e"
  :recipe (:host github
           :repo "tecosaur/screenshot"
           :branch "master"))

(package! org-pandoc-import 
  :pin "db308f1a05be26ce5b287633637ce554599b1377"
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! kdl-ts-mode
  :pin "3dbf116cd19261d8d70f456ae3385e1d20208452"
  :recipe (:host github
           :repo "dataphract/kdl-ts-mode"
           :branch "main"))

;; org roam ui
(package! websocket)
(package! org-roam-ui
  :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab"
  :recipe (:host github
           :repo "org-roam/org-roam-ui"
           :build t
           :files ("*.el" "out")))

;; this is to get around the fact that the package version of this file is
;; hosted on a server that sometimes goes down
(package! json-process-client
  :pin "c4385859ada9b7803698a1f0199fea7fc8880214"
  :recipe (:host github
           :repo "emacsmirror/json-process-client"))

;;; ~/.doom.d/scripts/org-templates.el -*- lexical-binding: t; -*-
;; read about org capture templates here:
;; https://orgmode.org/manual/Capture-templates.html

(setq self/reading-capture-list-item-template "** [ ] %(org-cliplink-capture)\n")
(setq self/reading-capture-find-file-template "** [ ] %(self/capture-insert-file-link)")

(setq self/org-roam-default-file-name-template "%<%Y%m%d%H%M%S>-${slug}")
(setq self/org-roam-default-file-head-template "#+title: ${title}\n")

;; TODO add Roam capture templates
(setq org-capture-templates
      `(("r" "Reading")
        ("ri" "Insert link")
        ("ria" "Insert article" entry (file+headline "~/org/reading.org" "Articles")
         ,self/reading-capture-list-item-template :immediate-finish t)
        ("rib" "Insert book" entry (file+headline "~/org/reading.org" "Books")
         ,self/reading-capture-list-item-template :immediate-finish t)
        ("riv" "Insert video" entry (file+headline "~/org/reading.org" "Videos")
         ,self/reading-capture-list-item-template :immediate-finish t)
        ("rir" "Insert recipe" item (file+headline "~/org/recipes.org" "Recipes")
         "%(org-cliplink-capture)\n" :immediate-finish t)
        ("rf" "Insert file")
        ("rfa" "Insert article" entry (file+headline "~/org/reading.org" "Articles")
         ,self/reading-capture-find-file-template :immediate-finish t)
        ("rfb" "Insert book" entry (file+headline "~/org/reading.org" "Books")
         ,self/reading-capture-find-file-template :immediate-finish t)))

(setq org-roam-capture-templates
      ; default
      `(("d" "default" plain #'org-roam-capture--get-point
         "%?"
         :file-name ,self/org-roam-default-file-name-template
         :head ,self/org-roam-default-file-head-template
         :unnarrowed t)

        ; permanent
        ("p" "permanent" plain #'org-roam-capture--get-point
         "- tags :: %?"
         :file-name ,self/org-roam-default-file-name-template
         :head ,self/org-roam-default-file-head-template
         :unnarrowed t)

        ; literature
        ("l" "literature" plain #'org-roam-capture--get-point
         "- source ::

* Notes
%?"
         :file-name ,(format "literature/%s" self/org-roam-default-file-name-template)
         :head ,self/org-roam-default-file-head-template
         :unnarrowed t)

        ; literature from link
        ("L" "literature from link" plain #'org-roam-capture--get-point
         "#+roam_key: %(car kill-ring-yank-pointer)
- source :: %(org-cliplink-capture)

* Notes
%?"
         :file-name ,(format "literature/%s" self/org-roam-default-file-name-template)
         :head ,self/org-roam-default-file-head-template
         :unnarrowed t)

        ; category notes. like default notes, but by default immediately finish
        ("c" "category" plain #'org-roam-capture--get-point
         "%?"
         :file-name ,self/org-roam-default-file-name-template
         :head ,self/org-roam-default-file-head-template
         :unnarrowed t
         :immediate-finish t)
        ))

(setq org-roam-dailies-capture-templates
      '(("d" "daily" plain (function org-roam-capture--get-point) ""
         :file-name "daily/%<%Y%m%d>"
         :head "#+title: %<%Y%m%d>")))

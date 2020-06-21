;;; ~/.doom.d/scripts/org-templates.el -*- lexical-binding: t; -*-
;; read about org capture templates here:
;; https://orgmode.org/manual/Capture-templates.html

(setq original-capture-templates org-capture-templates)

;; for debugging capture templates...
(defun reset-capture-templates ()
    (setq org-capture-templates original-capture-templates))

(add-to-list 'org-capture-templates
              '("r" "Reading"))
(add-to-list 'org-capture-templates
              '("ri" "Insert link"))
(add-to-list 'org-capture-templates
             '("ria" "Insert article" entry (file+headline "~/org/reading.org" "Articles")
              "** [ ] %(org-cliplink-capture)\n" :immediate-finish t))
(add-to-list 'org-capture-templates
             '("rib" "Insert book" entry (file+headline "~/org/reading.org" "Books")
              "** [ ] %(org-cliplink-capture)\n" :immediate-finish t))

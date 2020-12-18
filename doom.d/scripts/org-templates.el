;;; ~/.doom.d/scripts/org-templates.el -*- lexical-binding: t; -*-
;; read about org capture templates here:
;; https://orgmode.org/manual/Capture-templates.html

(setq original-capture-templates org-capture-templates)

;; for debugging capture templates...
(defun reset-capture-templates ()
    (setq org-capture-templates original-capture-templates))

;; (add-to-list 'org-capture-templates
;;               '("r" "Reading"))
;; (add-to-list 'org-capture-templates
;;               '("ri" "Insert link"))
;; (add-to-list 'org-capture-templates
;;              '("ria" "Insert article" entry (file+headline "~/org/reading.org" "Articles")
;;               "** [ ] %(org-cliplink-capture)\n" :immediate-finish t))
;; (add-to-list 'org-capture-templates
;;              '("rib" "Insert book" entry (file+headline "~/org/reading.org" "Books")
;;               "** [ ] %(org-cliplink-capture)\n" :immediate-finish t))

(setq self/reading-capture-list-item-template "** [ ] %(org-cliplink-capture)\n")

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
        ("n" "Note")
        ("np" "With page number" item (file+headline "~/org/inbox.org" "Inbox")
         "- %U\n  source: \n  p. \n  %?" :prepend t)
        ("nu" "Without page number" item (file+headline "~/org/inbox.org" "Inbox")
         "- %U\n  source: \n  %?" :prepend t)))

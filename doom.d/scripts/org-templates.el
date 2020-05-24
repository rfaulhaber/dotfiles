;;; ~/.doom.d/scripts/org-templates.el -*- lexical-binding: t; -*-
;; read about org capture templates here:
;; https://orgmode.org/manual/Capture-templates.html

(add-to-list 'org-capture-templates
              '("r" "Reading")
                )
(add-to-list 'org-capture-templates
              '("ra" "Annotation" entry (file "~/org/notes/books/refile.org")
                "** %T\n%?"
                :empty-lines 1))
(add-to-list 'org-capture-templates
              '("rc" "New chapter" entry (file "~/org/notes/books/refile.org")
                "* %?"
                :empty-lines 1))

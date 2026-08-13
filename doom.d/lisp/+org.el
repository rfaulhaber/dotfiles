;;; $DOOMDIR/lisp/+org.el -*- lexical-binding: t; -*-

;; capture templates
(after! org
  (load! "./+org-capture"))

;; for org mode, set the fill column to 130
(add-hook 'org-mode-hook (lambda ()
                           (setq-local fill-column 130)))

;; GPG key used by org-crypt; the :lang org +crypt flag wires up the rest
(setq org-crypt-key "A90BC7B722983F6BB8EAC1DA144A6B5FBB68FC9D")

;; org-md
;; the default md exporter for source code blocks is bad, so we replace it
(advice-add 'org-md-example-block :override #'self/org-md-src-block)

;; the default md exporter should always unfold paragraphs
(advice-add 'org-md-paragraph :filter-args #'self/org-md-paragraph-unfill)

;; org-agenda
(setq org-agenda-files
      (mapcar
       (lambda (str)
         (concat org-directory "/" str))
       '("todo.org"
         "habits.org"
         "blog.org"
         "todo")))

;; org-roam
(setq org-roam-directory "~/org/roam")

;; for adding backlinks to exported org-roam files
(add-hook 'org-export-before-processing-functions #'self/org-roam-export-refs)

;; org-roam-ui
(setq org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t
      org-roam-ui-open-on-start t)

;; org-modern
(setq org-modern-list
      '((?* . "•")
        (?+ . "‣"))
      org-modern-fold-stars
      '(("◉" . "○")
        ("◉" . "○")
        ("▸" . "▾")
        ("▸" . "▾"))
      org-modern-checkbox
      '((?X . "󱗼")
        (?- . "┅")
        (?\s . " ")))

;; org-journal
(setq org-journal-dir "~/org/journal"
      org-journal-file-format "%Y%m%d.org")

(add-hook 'org-journal-mode-hook (lambda ()
                                   (setq org-element-use-cache nil)))

;; org-ref
(setq bibtex-completion-notes-path "~/org/bibliography/notes.org"
      bibtex-completion-bibliography '("~/org/bibliography/references.bib"))

;; sometimes org publish complains about not being able to resolve ids. This is
;; a workaround for that
(advice-add 'org-publish :before #'self/org-publish-before-advice)

;; org-publish
;; TODO add hooks for publishing roam files
(setq org-publish-project-alist '(("roam web"
                                   :base-directory "~/org/roam"
                                   :base-extension "org"
                                   :publishing-directory "~/Projects/roam-web"
                                   :publishing-function org-html-publish-to-html
                                   :with-author nil
                                   :recursive t)))

;; dynamically load languages for org-babel
;; thank you r/emacs: https://www.reddit.com/r/emacs/comments/us7zae/comment/i9ceaco
(advice-add 'org-babel-execute-src-block :around #'self/org-babel-execute-src-block-lazy-load)

;; see: https://github.com/hlissner/doom-emacs/issues/3185
(advice-add '+org-inline-image-data-fn :override #'self/+org-inline-image-data-fn)

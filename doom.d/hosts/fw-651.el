;;; ../Sources/dotfiles/doom.d/hosts/fw-199.el -*- lexical-binding: t; -*-

;; config variables
(setq config/font-size 16
      config/work-computer-p t)

;; set window size
(when (window-system)
  (set-frame-size (selected-frame) 120 50))

;; org-journal config
(setq org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STRT\"")
(add-hook 'org-journal-after-header-create-hook #'work/org-journal-after-header-create-hook)

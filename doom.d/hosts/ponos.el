;;; ../Sources/dotfiles/doom.d/hosts/fw-199.el -*- lexical-binding: t; -*-

;; config variables
(setq config/font-size 16
      config/work-computer-p t)

;; set window size
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 50))

;; org-journal config
(add-hook 'org-journal-after-header-create-hook #'work/org-journal-after-header-create-hook)

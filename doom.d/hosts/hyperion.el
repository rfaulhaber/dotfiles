;;; $DOOMDIR/hosts/hyperion.el -*- lexical-binding: t; -*-

;; set font size
(setq config/font-size 24)

;; on hyperion I have racket installed globally. as of writing this it is the only
;; scheme I have installed globally!
(setq geiser-active-implementations '(racket))

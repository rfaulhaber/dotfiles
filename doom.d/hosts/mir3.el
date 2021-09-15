;;; $DOOMDIR/hosts/mir3.el -*- lexical-binding: t; -*-

;; set font size
(setq config/font-size 24)

;; on mir3 I have racket installed globally. as of writing this it is the only
;; scheme I have installed globally!
(setq geiser-active-implementations '(racket))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defvar config/font-size 16 "Font size. Should be specified by a host.")
(defvar self/system-name (string-trim-right (system-name) (rx (or "\.lan" "\.attlocal.net" "\.local")))
  "System name. Used in loading init scripts.")
(defvar self/system-type (pcase system-type
                           ('gnu/linux "linux")
                           ('darwin "darwin"))
  "System type. Either 'linux' or 'darwin'.
Used in loading config specific to those systems.")

(message "loading configuration for %s on system %s"
         self/system-name
         self/system-type)

;; load type-specific and machine-specific configs
;; machine-specific configs can override type-specific configs
(load! (format "./hosts/%s" self/system-type) nil t)
(load! (format "./hosts/%s" self/system-name) nil t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Faulhaber"
      user-mail-address "ryf@sent.as"
      calendar-latitude 41.49
      calendar-longitude -81.69
      calendar-location-name "Cleveland, OH")

(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size config/font-size))

(setq doom-theme 'doom-tokyo-night)

(setq display-line-numbers-type t)

;; must be set before org loads
(setq org-directory "~/org")

;; topic-specific config; interactive commands and helpers live in ./autoload
;; and are lazy-loaded through doom's generated autoloads
(load! "./lisp/+bindings")
(load! "./lisp/+org")
(load! "./lisp/+treesit")
(load! "./lisp/+nix")
(load! "./lisp/+langs")
(load! "./lisp/+tools")

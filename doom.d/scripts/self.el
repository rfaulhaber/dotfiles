;;; ~/.doom.d/scripts/self.el -*- lexical-binding: t; -*-

;; custom scripts for misc personal use

(defun self/eww-open-url-window-right (url)
  "Opens URL in eww-mode in a new window to the right."
  (interactive "sURL: ")
  (let* ((new-buf-name (format "*%s*" url))
         (new-window (split-window-right)))
    (with-selected-window new-window
      (switch-to-buffer new-buf-name)
      (eww-mode)
      (eww url))))

;; thank you github.com/hrs for the inspiration
(defun self/new-scratch-buffer ()
  "Creates and opensa new scratch buffer with a random name"
  (interactive)
  (let ((new-window (split-window-below)))
    (with-selected-window new-window
      (switch-to-buffer (format "*%s*" (make-temp-name "scratch-"))))))

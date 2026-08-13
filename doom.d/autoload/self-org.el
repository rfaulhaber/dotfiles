;;; $DOOMDIR/autoload/self-org.el -*- lexical-binding: t; -*-

;; org, org-roam, and org-export helpers; the advice and hooks that use these
;; are registered in ../lisp/+org.el

;; this comes from reddit. thank you r/emacs!
;;;###autoload
(defun self/org-md-paragraph-unfill (&rest args)
  "Unfill CONTENTS, the `cadr' in ARGS."
  (let* ((actual-args (car args))
         (org-el (nth 0 actual-args))
         (contents (nth 1 actual-args))
         (info (nth 2 actual-args)))
    ;; Unfill contents
    (unless (eq (car org-el) 'src-block)
      (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
    (list org-el contents info)))

;;;###autoload
(defun self/capture-insert-file-link ()
  "Imitation of org-insert-link but for use in org-capture template"
  (let* ((file-path (read-file-name "File: "))
         (file-name (read-from-minibuffer "Description: ")))
    (format "[[%s][%s]]" file-path file-name)))

;; thank you doom emacs discord user zzamboni
;; https://discordapp.com/channels/406534637242810369/695219268358504458/788524346309214249
;;;###autoload
(defun self/org-md-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (or (org-element-property :language src-block) "")))
    (format "```%s\n%s```\n"
            lang
            (org-remove-indentation
             (org-export-format-code-default src-block info)))))

;; TODO write more generic roam exporter that extends org publishing
;;;###autoload
(defun self/org-roam-export-refs (_backend)
  "For org-roam files, exports the ROAM_REF property as a section at the bottom
of the file as an unordered list."
  (save-excursion
    (goto-char (point-min))
    (when (and
           (org-roam-file-p)
           (not (eq (assoc "ROAM_REFS" (org-entry-properties)) nil)))
      (goto-char (point-min))
      (let* ((file-refs (split-string (cdr (assoc "ROAM_REFS" (org-entry-properties))) " "))
             (refs-as-bullet-links (mapcar
                                    (lambda (link)
                                      (format "- [[%s]]\n" link))
                                    file-refs)))
        (unless (or
                 (eq refs-as-bullet-links nil)
                 (eq (length refs-as-bullet-links) 0))
          (goto-char (point-max))
          (insert (concat "\n* Refs\n") (apply 'concat refs-as-bullet-links)))))))

;;;###autoload
(defun self/roam-ref-add-from-clipboard ()
  (interactive)
  (org-roam-ref-add (car kill-ring)))

;;;###autoload
(defun self/org-publish-before-advice (&rest args)
  (org-roam-update-org-id-locations))

;; NOTE doesn't quite work?
;;;###autoload
(defun self/org-babel-execute-src-block-lazy-load (original-fn &rest args)
  (let ((lang (org-element-property :language (org-element-at-point))))
    (when (or (string= lang "bash") (string= lang "sh"))
      (setq lang "shell"))
    (unless (or (not (boundp 'org-babel-load-languages)) (cdr (assoc (intern lang) org-babel-load-languages)))
      (add-to-list 'org-babel-load-languages (cons (intern lang) t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    (apply original-fn args)))

;;;###autoload
(defun self/+org-inline-image-data-fn (_original-fn &rest args)
  (cl-destructuring-bind (_ link) args
    (with-demoted-errors "%S" (base64-decode-string link))))

(require 'ox)

(org-export-define-derived-backend 'work-journal 'blackfriday
                                   :translate-alist '((headline . org-work-journal-headline))
                                   :menu-entry '(?f "As Work Journal"
                                                    ((?F "To temporary buffer" (lambda (a s v b)
                                                                                 (org-work-journal-export-as-work-journal a s v))))))

(defun org-work-journal-headline (headline contents info)
  "Converts an org headline into bold Slack markdown."
  (let ((title (org-export-data (org-element-property :title headline) info)))
    (format "*%s*\n%s" title (or contents ""))))

;;;###autoload
(defun org-work-journal-export-as-work-journal (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'work-journal "*Work Journal*" async subtreep visible-only nil nil (lambda () (text-mode))))

(provide 'work-journal)

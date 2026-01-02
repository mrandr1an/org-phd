;;; org-phd-buffer --- Summary
;;; Commentary:
;;; Code:
(defgroup org-phd-buffer nil
  "Group for org-phd-buffer."
  :group 'org-phd
)

(defun org-phd-buffer/get-buffer ()
  "Create an org-phd buffer based on the #+TITLE."
  (let* ((title (or (org-get-title) "Untitled"))
	 (buffer-name (format "*%s.phd*" title)))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq buffer-read-only t)
      (current-buffer)))
)

(defun org-phd-buffer/send-warning (content &optional buffer)
  "CONTENT BUFFER."
)

(defun org-phd-buffer/send-msg (content &optional buffer)
  "CONTENT BUFFER."
)

(defun org-phd-buffer/send-error (content &optional buffer)
  "CONTENT BUFFER."
)

(defun org-phd-buffer/export (&optional buffer)
  "CONTENT BUFFER."
)

(provide 'org-phd-buffer)
;;; org-phd-buffer.el ends here

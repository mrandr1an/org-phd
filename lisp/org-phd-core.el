(defgroup org-phd nil
  "Group for org-phd."
  :group 'org
)

(defcustom org-phd-types '(publication note blog agenda)
  "Types of org-phd content."
  :type '(repeat symbol)
  :group 'org-phd
)

(defcustom org-phd-targets '(pdf html)
  "Targets of org-phd."
  :type '(repeat symbol)
  :group 'org-phd
)

(defun org-phd-buffer/get-buffer ()
  "."
  (get-buffer-create "*Org PhD*")
)

(defun org-phd-core/export (&optional buffer type target)
  "BUFFER is."
  (interactive)
  (if (called-interactively-p 'any)
      (org-phd-export--interactive)
      (org-phd-export--noninteractive))
)

(defun org-phd-export--interactive ()
  "."
  (message "Interactive")
)

(defun org-phd-export--noninteractive ()
  "."
  (message "Not Interactive")
)

(provide 'org-phd-core)
;;; org-phd-core.el ends here

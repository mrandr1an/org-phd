;;; org-phd-core --- Summary
;;; Commentary:
;;; Code:
(defgroup org-phd nil
  "Group for org-phd."
  :group 'org
)

(defcustom org-phd-types '(publication note blog agenda)
  "Types of org-phd content."
  :type '(repeat symbol)
  :group 'org-phd
)

(defcustom org-phd-targets '(pdf html github-md)
  "Targets of org-phd."
  :type '(repeat symbol)
  :group 'org-phd
)

(defun org-phd-core/export (&optional buffer type target)
  "Export BUFFER as TYPE into TARGET."
  (interactive)
  (if (called-interactively-p 'any)
      (org-phd-export--interactive)
      (org-phd-export--noninteractive))
)

(defun org-phd-export--interactive ()
  "."
  (message "Super Interactive")
)

(defun org-phd-export--noninteractive ()
  "."
  (message "Not Interactive")
)


(provide 'org-phd-core)
;;; org-phd-core.el ends here

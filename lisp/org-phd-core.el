(defgroup org-phd nil
  "Group for org-phd."
  :group 'org
)

(defcustom org-phd-types '(publication note blog agenda)
  "Types of org-phd content."
  :type '(repeat symbol)
  :group 'org-phd
)

(defun org-phd-read-org ()
  "Test function."
  (interactive)
  (let ((key (org-collect-keywords '("TITLE"))))
    (message "%S" key))
)

(provide 'org-phd-core)
;;; org-phd-core.el ends here

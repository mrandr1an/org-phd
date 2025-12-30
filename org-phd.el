;;; org-phd --- org-phd.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.2"))
;; Keywords: Convenience, Project Management, Writting
;;; Commentary:
;;; Code:

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "lisp" (file-name-directory (or load-file-name buffer-file-name)))))

;;;###autoload
(define-minor-mode org-phd-minor-mode
  "Test description."
  :init-value nil
  :lighter " org-phd"
  (if org-phd-minor-mode
      (progn
	(require 'org-phd-core)
	(message "org-phd minor mode enabled"))
    (message "org-phd minor mode disabled"))
 )

(provide 'org-phd)
;;; org-phd.el ends here

(use-package org-phd
  :elpaca (:host github :repo "mrandr1an/org-phd")
  :commands (org-phd-minor-mode)
)

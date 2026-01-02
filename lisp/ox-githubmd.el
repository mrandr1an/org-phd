;;; ox-githubmd --- Summary
;;; Commentary:
;;; Code:

(org-export-define-backend 'github-md
  '(
    (headline . org-phd-ox/github-md/headline)
    (section . org-phd-ox/github-md/section)
    (paragraph . org-phd-ox/github-md/paragraph)

    (bold . org-phd-ox/github-md/bold)
    (strike-through . org-phd-ox/github-md/strike-through)
    (italic . org-phd-ox/github-md/italic)
    (underlined . org-phd-ox/github-md/underlined)
    (latex-fragment. org-phd-ox/github-md/latex-fragment)
   )
 :menu-entry
  '(?g "Export to GitHub Markdown"
       ((?f "To file"   github-md-export-to-file)
        (?b "To buffer" github-md-export-to-buffer)))
)

(defun github-md-export-to-file (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a .md file using the github-md backend."
  (interactive)
  (let* ((infile (or (buffer-file-name) "org-export"))
         (outfile (concat (file-name-sans-extension infile) ".md")))
    (org-export-to-file 'github-md outfile
      async subtreep visible-only body-only ext-plist)))

(defun github-md-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a buffer using the github-md backend."
  (interactive)
  (org-export-to-buffer 'github-md "*Org GitHub MD Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (gfm-mode))))

(defun org-phd-ox/github-md/headline (headline contents info)
  "Translate an org HEADLINE into a github-markdown headline.
CONTENTS holds the contents of the headline.  INFO is a plist holding
contextual information."
  (let* (
	 (level (org-export-get-relative-level headline info))
	 (title (org-export-data (org-element-property :title
						       headline) info ))
	 (hashes (make-string level ?#)))
    (concat hashes " " title "\n" (when contents (concat contents "\n")))
  )
)

(defun org-phd-ox/github-md/section (section contents info)
  "Translate an org SECTION into a github markdown section.
CONTENTS holds the contents of the paragraph.  INFO is a plist holding
contextual information."
  (when contents
  (concat (string-trim-right contents) "\n\n"))
)

(defun org-phd-ox/github-md/paragraph (paragraph contents info)
  "Translate an org PARAGRAPH into a github markdown paragraph.
CONTENTS holds the contents of the paragraph.  INFO is a plist holding
contextual information."
  (when contents
    (concat (string-trim-right contents) "\n"))
)

(defun org-phd-ox/github-md/bold (bold contents info)
  "Translate an org BOLD into a github markdown bold.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (format "**%s**" (or contents ""))
)

(defun org-phd-ox/github-md/underline (_underline contents _info)
  "Translate Org UNDERLINE into GitHub Markdown using HTML."
  (format "<u>%s</u>" (or contents "")))

(defun org-phd-ox/github-md/strike-through (_strike-through contents _info)
  "Translate Org STRIKE-THROUGH into GitHub Markdown."
  (format "~~%s~~" (or contents "")))

(defun org-phd-ox/github-md/italic (_italic contents _info)
  "Translate Org ITALIC into GitHub Markdown."
  (format "*%s*" (or contents "")))

(defun org-phd-ox/github-md/latex-fragment (latex-fragment _contents _info)
  "Translate Org LaTeX fragment into inline code."
  (let ((value (org-element-property :value latex-fragment)))
    (format "$%s$" value)))

(defun org-phd-ox/github-md/table (table contents info)
)

(defun org-phd-ox/github-md/table-row (table-row contents info)
)

(defun org-phd-ox/github-md/table-cell (table-cell contents info )

)

(defun org-phd-ox/github-md/link (link contents info)

)

(defun org-phd-ox/github-md/src-block (src-block contents info)
)

(defun org-phd-ox/github-md/special-block (special-block contents
							 info)
)

(provide 'ox-githubmd)
;;; ox-githubmd.el ends here

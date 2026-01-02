;;; ox-githubmd --- Summary
;;; Commentary:
;;; Code:

(org-export-define-backend 'github-md
  '(
    (headline . org-phd-ox/github-md/headline)
    (section . org-phd-ox/github-md/section)
    (paragraph . org-phd-ox/github-md/paragraph)

    (src-block . org-phd-ox/github-md/src-block)
    (special-block . org-phd-ox/github-md/special-block)

    (link . org-phd-ox/github-md/link)

    (bold . org-phd-ox/github-md/bold)
    (strike-through . org-phd-ox/github-md/strike-through)
    (italic . org-phd-ox/github-md/italic)
    (underline . org-phd-ox/github-md/underline)
    (code . org-phd-ox/github-md/code)
    (latex-fragment . org-phd-ox/github-md/latex-fragment)

    (template . org-phd-ox/github-md/template)
   )
 :options-alist
 '(
   (:subtitle "SUBTITLE" nil "" t)
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

(defun org-phd-ox/github-md/template (contents info)
  (let* ((title (org-export-data (plist-get info :title) info))
         (subtitle (org-export-data (plist-get info :subtitle) info))
         (title (org-html-encode-plain-text (or title "")))
         (subtitle (org-html-encode-plain-text (or subtitle "")))
         (header
          (cond
           ((and (string-empty-p title) (string-empty-p subtitle))
            "")
           ((string-empty-p subtitle)
            (format "<div align=\"center\">\n\n# %s\n\n</div>\n\n" title))
           (t
            (format "<div align=\"center\">\n\n# %s\n\n%s\n\n</div>\n\n"
                    title subtitle)))))
    (concat header contents)))

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
  "Translate Org UNDERLINE into GitHub Markdown using HTML.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (format "<u>%s</u>" (or contents "")))

(defun org-phd-ox/github-md/strike-through (_strike-through contents _info)
  "Translate Org STRIKE-THROUGH into GitHub Markdown.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (format "~~%s~~" (or contents "")))

(defun org-phd-ox/github-md/italic (_italic contents _info)
  "Translate Org ITALIC into GitHub Markdown.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (format "*%s*" (or contents "")))

(defun org-phd-ox/github-md/code (code _contents _info)
  "Translate Org inline CODE into GitHub Markdown."
  (let ((value (org-element-property :value code)))
    (format "`%s`" (or value ""))))

(defun org-phd-ox/github-md/latex-fragment (latex-fragment _contents _info)
  "Translate Org LATEX-FRAGMENT into inline code.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (let ((value (org-element-property :value latex-fragment)))
    (format "$%s$" value)))

(defun org-phd-ox/github-md/table (table contents info)
)

(defun org-phd-ox/github-md/table-row (table-row contents info)
)

(defun org-phd-ox/github-md/table-cell (table-cell contents info )

)

(defun org-phd-ox/github-md/link (link contents info)
  "Translate an Org LINK object into GitHub-flavored Markdown."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (desc (or contents
                   (org-element-property :raw-link link)
                   raw-path
                   "")))
    (pcase type
      ;; External-ish links
      ((or "http" "https" "ftp" "mailto")
       (let ((url (org-element-property :raw-link link)))
         (if (and contents (not (string-empty-p contents)))
             (format "[%s](%s)" desc url)
           ;; Autolink looks nicer when there's no description
           (format "<%s>" url))))

      ;; File links: prefer a relative path without the "file:" prefix
      ("file"
       (let ((path raw-path))
         ;; org stores file path in :path (no "file:" prefix), but it may be URL-escaped
         (setq path (replace-regexp-in-string "%20" " " path))
         (if (and contents (not (string-empty-p contents)))
             (format "[%s](%s)" desc path)
           (format "`%s`" path))))

      ;; In-buffer fuzzy links like [[*Heading]] or [[Heading]]
      ("fuzzy"
       ;; GitHub anchor generation is not 1:1 with Org headings; best-effort:
       ;; if there's a description, keep it; otherwise show plain text.
       (if (and contents (not (string-empty-p contents)))
           desc
         (format "`%s`" raw-path)))

      ;; IDs/custom types: can't resolve without extra machinery; degrade gracefully
      (_
       (if (and contents (not (string-empty-p contents)))
           desc
         (format "`%s:%s`" type raw-path))))))

(defun org-phd-ox/github-md/src-block (src-block _contents _info)
  "Translate an Org SRC-BLOCK element into a GitHub Markdown fenced code block."
  (let* ((lang (org-element-property :language src-block))
         (code (or (org-element-property :value src-block) "")))
    (concat "```" (or lang "") "\n"
            code
            (unless (string-suffix-p "\n" code) "\n")
            "```\n\n")))

(defun org-phd-ox/github-md/special-block (special-block contents
							 info)
  "Translate Org SPECIAL-BLOCK into GitHub Markdown.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (let* ((type (org-element-property :type special-block))
	 (label (upcase type)))
    (when contents
      (concat "> [!" label "]\n"  (mapconcat (lambda (line) (concat "> " line))
                         (split-string (string-trim-right contents) "\n")
                         "\n"))
      ))
)

(provide 'ox-githubmd)
;;; ox-githubmd.el ends here

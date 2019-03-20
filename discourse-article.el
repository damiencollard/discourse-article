;;; discourse-article.el --- Treatments for Discourse e-mails

;; Copyright (c) 2019 Damien Collard

;; Author: Damien Collard <damien.collard@laposte.net>
;; URL:
;; Version: 0.0.1
;; Keywords: gnus mail convenience
;; Package-Requires: ((emacs "24.3") (gnus "5.13"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module contains treatments for articles coming from
;; Discourse forums. The functions are meant to be added to
;; `gnus-treatment-function-alist'.

;;; Code:

(require 'gnus-art)

(defun is-discourse-mail ()
  "Return whether an e-mail is from a Discourse forum."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^From: .*@discoursemail.com" nil t)))

;; TODO: Make links clickable! See
;; <https://www.gnu.org/software/emacs/manual/html_node/elisp/Clickable-Text.html>
(defun treat-discourse-links ()
  "Make Discourse links clickable.

A Discourse link has the form:

    [LABEL](URL)

This function replaces such a link with: LABEL, where LABEL is
highlighted with the `link' face and when clicked, follows the
link to URL."
  (interactive)
  (when (is-discourse-mail)
    (let ((inhibit-read-only t))
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\\[\\([^\]]+\\)\\](\\([^)]+\\))" nil t)
            (let* ((beg (match-beginning 0))
                   (end (match-end 0))
                   (label (match-string 1))
                   (cleaned-label (replace-regexp-in-string "\n" " " label))
                   (url (match-string 2)))
              (delete-region beg end)
              (insert (propertize cleaned-label
                                  'face 'link
                                  'mouse-face 'highlight
                                  'help-echo "Follow the link"))
              (fill-paragraph))))))))

(defun discourse-article-transform-quotes ()
  "Replace Discourse quotes with mail-style citations.

A Discourse quote has the form:

    `[quote=\"AUTHOR,...\"]TEXT[/quote]`.

This function replaces such a quote with:
    AUTHOR wrote:
    > TEXT
    > TEXT
    > TEXT
and highlights the whole with face `gnus-cite-1'.

Must be added to `gnus-treatment-function-alist' *before*
the `nice-citation' treatment, if present."
  (interactive)
  (when (is-discourse-mail)
    (let ((inhibit-read-only t)
          (article-fill-column fill-column))
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\[quote=\"\\([^\"]*\\)\"\\]\\([^[]*\\)\\[/quote\\]" nil t)
            (let* ((beg (match-beginning 0))
                   (end (match-end 0))
                   (from (match-string 1))
                   (text (match-string 2))
                   (author (progn (string-match "^\\([^,]+\\)," from)
                                  (match-string 1 from)))
                   (new-from (concat author " wrote:"))
                   (new-text (with-temp-buffer
                               (insert text)
                               (let ((fill-column article-fill-column))
                                 (fill-region (point-min) (point-max)))
                               (goto-char (point-min))
                               (while (< (point) (point-max))
                                 (insert "> ")
                                 (forward-line))
                               (buffer-string))))
              (delete-region beg end)
              (goto-char beg)
              (insert
               (concat (propertize new-from 'face '(:inherit gnus-cite-1 :slant italic))
                       "\n"
                       (propertize new-text 'face 'gnus-cite-1)
                       "\n")))))))
    ;; ;; FIXME: Remove the call to nice-citation once
    ;; ;; we add `treat-discourse-quotes' in the treatment list
    ;; ;; (right before nice-citation)
    ;; (nice-citation-apply)
    ))

(defcustom discourse-article-treat-quotes t
  "Replace Discourse-style quotes with mail-style ones.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-articel-treat-quotes 'highlight t)

;; IMPORTANT: Must be done *before* `nice-citation' (if present) does the same
;; as otherwise nice-citation would run before the Discourse quotes treatment
;; and thus it would leave these quotes un-prettified.
(nconc gnus-treatment-function-alist
             '((discourse-article-treat-quotes discourse-article-treansform-quotes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight code between triple backticks.
;;
;; FIXME: Although it does highlight fenced code properly, it does not
;; highlight inline code and, more annoying, it removes the highlighting of the
;; article. (And the highlighting of the language keyword and triple backticks
;; stands out way too much.)

(defcustom pm-host/article
  (pm-host-chunkmode :name "article"
                     :mode 'article-mode)
  "Article host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

(defcustom pm-inner/article-fenced-code
  (pm-inner-auto-chunkmode :name "article-fenced-code"
                           :head-matcher "^[ \t]*```[{ \t]*\\w.*$"
                           :tail-matcher "^[ \t]*```[ \t]*$"
                           :mode-matcher (cons "```[ \t]*{?\\(?:lang *= *\\)?\\([^ \t\n;=,}]+\\)" 1)
                           :head-mode 'markdown-mode
                           :tail-mode 'markdown-mode)
  "Markdown fenced code block."
  :group 'poly-innermodes
  :type 'object)

(defcustom pm-inner/article-inline-code
  (pm-inner-auto-chunkmode :name "article-inline-code"
                           :head-matcher (cons "[^`]\\(`{?[[:alpha:]+-]+\\)[ \t]" 1)
                           :tail-matcher (cons "[^`]\\(`\\)[^`]" 1)
                           :mode-matcher (cons "`[ \t]*{?\\(?:lang *= *\\)?\\([[:alpha:]+-]+\\)" 1)
                           :head-mode 'host
                           :tail-mode 'host)
  "Markdown inline code."
  :group 'poly-innermodes
  :type 'object)

(define-polymode poly-article-mode
  :hostmode 'pm-host/article
  :innermodes '(pm-inner/article-fenced-code
                pm-inner/article-inline-code))

(provide 'discourse-article)
;;; discourse-article.el ends here

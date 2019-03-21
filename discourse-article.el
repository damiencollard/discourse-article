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
(require 'xml)

;; TODO: Add variable holding a list of From regexes used to detect whether an e-mail
;; is from a Discourse forum?

(defun discourse-article--is-discourse ()
  "Return whether an e-mail is from a Discourse forum.
Detection is based on the `From` field matching `@discoursemail.com`."
  (gnus-with-article-buffer
    (save-excursion
      (save-restriction
        (widen)
        (article-narrow-to-head)
        ;; XXX: There must be some gnus- or message- function to get the From header?
        (re-search-forward "^From: .*@discoursemail.com" nil t)))))

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
  (when (discourse-article--is-discourse)
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
                               (save-excursion (insert text))
                               (let ((fill-column article-fill-column))
                                 (fill-region (point-min) (point-max)))
                               (save-excursion
                                 (while (< (point) (point-max))
                                   (insert "> ")
                                   (forward-line)))
                               ;; Discourse quotes may contain HTML entities.
                               (xml-parse-string))))
              (delete-region beg end)
              (goto-char beg)
              (insert
               (concat (propertize new-from 'face '(:inherit gnus-cite-1 :slant italic))
                       "\n"
                       (propertize new-text 'face 'gnus-cite-1)
                       "\n")))))))))

(defcustom discourse-article-treat-quotes t
  "Replace Discourse-style quotes with mail-style ones.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-article-treat-quotes 'highlight t)

;; IMPORTANT: Must be done *before* `nice-citation' (if present) does the same
;; as otherwise nice-citation would run before the Discourse quotes treatment
;; and thus it would leave these quotes un-prettified.
(nconc gnus-treatment-function-alist
             '((discourse-article-treat-quotes discourse-article-transform-quotes)))

(defun discourse-article-space-out-code-blocks ()
  "Ensure fenced code blocks are separate paragraphs.
Fenced code blocks are delimited by triple backticks.
A newline is inserted before and after, if needed."
  (interactive)
  (when (discourse-article--is-discourse)
    ;; Space out the code blocks.
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (let ((in-code-block nil))
          (while (re-search-forward "^```" nil t)
            (setq in-code-block (not in-code-block))
            (if in-code-block
                (save-excursion
                  (beginning-of-line)
                  (when (not (looking-back "\n\n"))
                    (insert "\n")))
              (when (not (looking-at "\n\n"))
                (insert "\n")))))))))

(defun discourse-article--count-citation-marks ()
  "Count citation marks `>` in the current paragraph.
Assumes it is called right before the paragraph, on an empty
line, as is typically the case when advancing with
`forward-paragraph`."
  (let ((count 0))
    (save-mark-and-excursion
      (forward-line) ;; Skip the empty line.
      (while (and (< (point) (point-max))
                  (not (looking-at "^$")))
        (when (looking-at "^ *>")
          (setq count (1+ count)))
        (forward-line)))
    count))

(defun discourse-article-fill-paragraphs ()
  "Fill all paragraphs except the fenced code blocks and the citations."
  (interactive)
  (when (discourse-article--is-discourse)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when (and (not (looking-at "\n*```"))
                     (= 0 (discourse-article--count-citation-marks)))
            (save-excursion
              (fill-paragraph)))
          (forward-paragraph))))))

(defun discourse-article-transform-paragraphs ()
  "Transform an e-mail's paragraphs.
Applies `discourse-article-space-out-code-blocks' followed by
`discourse-article-fill-paragraphs'. Refer to the doc of these
functions for details."
  (interactive)
  (discourse-article-space-out-code-blocks)
  (discourse-article-fill-paragraphs))

(defcustom discourse-article-treat-paragraphs nil
  "Fill-wrap paragraphs except the fenced code block ones and the citations.

This is *experimental* and may break paragraph formatting, in particular if
they contain non-fenced code, so it's off by default.

Note that when enabled, the treatment is only performed on e-mails detected as
coming from a Discourse forum (see `discourse-article--is-discourse').

Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-article-treat-paragraphs 'highlight t)

;; IMPORTANT: Must be done *after* `discourse-article-transform-quotes'.
;; And like `discourse-article-transform-quotes', it must also be performed
;; *before* `nice-citation'.
(nconc gnus-treatment-function-alist
             '((discourse-article-treat-paragraphs discourse-article-transform-paragraphs)))

(provide 'discourse-article)
;;; discourse-article.el ends here

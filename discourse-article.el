;;; discourse-article.el --- Treatments for Discourse e-mails

;; Copyright (c) 2019 Damien Collard

;; Author: Damien Collard <damien.collard@laposte.net>
;; URL:
;; Version: 0.0.2
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

(require 'dash)
(require 'gnus-art)
(require 'xml)

(defgroup discourse-article nil
  "Treatments for Discourse articles."
  :prefix "discourse-article-"
  :group 'gnus-article
  :link '(url-link :tag "GitHub" "https://github.com/damiencollard/discourse-article"))

(defcustom discourse-article-from-regexps '(".*@discoursemail.com")
  "Regexes matching addresses from which Discourse e-mails are received.
The Discourse Article treatments are only applied on incoming
e-mails if their From field matches one of the regexes."
  :group 'discourse-article
  :type '(repeat string))

(defcustom discourse-article-replies-beginning-delimiter "━"
  "String used to mark the beginning of previous replies.
This variable is used by `discourse-article-transform-previous-replies' which can
be controlled by `discourse-article-treat-previous-replies'."
  :group 'discourse-article
  :type '(choice (item :tag "None" :value nil)
		 string))

(defcustom discourse-article-inter-reply-delimiter "─"
  "String used to separate replies.
This variable is used by `discourse-article-transform-previous-replies' which can
be controlled by `discourse-article-treat-previous-replies'."
  :group 'discourse-article
  :type '(choice (item :tag "None" :value nil)
		 string))

(defface discourse-article-replies-delimiter-face
  '((t (:foreground "#464646")))
  "Face for the replies delimiters.
Applies to both the beginning and inter-reply delimiters."
  :group 'discourse-article)

(defface discourse-article-replies-heading-face
  '((t (:foreground "#e0e28c")))
  "Face for the replies heading text."
  :group 'discourse-article)

(defface discourse-article-reply-author-face
  '((t (:inherit gnus-header-from)))
  "Face for the replies' authors."
  :group 'discourse-article)

(defface discourse-article-section-face-1
  '((t (:inherit markdown-header-face-1)))
  "Face for titles of level 1 sections."
  :group 'discourse-article)

(defface discourse-article-section-face-2
  '((t (:inherit markdown-header-face-2)))
  "Face for titles of level 2 sections."
  :group 'discourse-article)

(defface discourse-article-section-face-3
  '((t (:inherit markdown-header-face-3)))
  "Face for titles of level 3 sections."
  :group 'discourse-article)

(defface discourse-article-section-face-4
  '((t (:inherit markdown-header-face-4)))
  "Face for titles of level 4 sections."
  :group 'discourse-article)

(defface discourse-article-section-face-5
  '((t (:inherit markdown-header-face-5)))
  "Face for titles of level 5 sections."
  :group 'discourse-article)

(defface discourse-article-section-face-6
  '((t (:inherit markdown-header-face-6)))
  "Face for titles of level 6 sections."
  :group 'discourse-article)

(defface discourse-article-code-background-face
  '((t (:background "#2e2e2e")))
  "Face for the background of fenced code blocks."
  :group 'discourse-article)

(defcustom discourse-article-code-background t
  "Whether to color the background of fenced code blocks.
The face is `discourse-article-code-background-face'."
  :group 'discourse-article
  :type 'boolean)

(defface discourse-article-user-face
  '((t (:foreground "pink")))
  "Face for Discourse users mentioned as @user."
  :group 'discourse-article)

(defun discourse-article--is-discourse ()
  "Return whether an e-mail is from a Discourse forum.
Determined by the regexes in `discourse-article-from-regexps'."
  (gnus-with-article-buffer
    (save-excursion
      (save-restriction
        (widen)
        (article-narrow-to-head)
        (gnus-article-goto-header "From")
        (let ((value (buffer-substring (point) (line-end-position))))
          (catch 'done
            (dolist (r discourse-article-from-regexps)
              (when (string-match r value)
                (throw 'done t)))))))))

(defun discourse-article-transform-previous-replies ()
  "Perform highlighting of the \"Previous Replies\" section."
  (interactive)
  (when (discourse-article--is-discourse)
    (let ((inhibit-read-only t)
          (start-sep (discourse-article--make-separator
                      discourse-article-replies-beginning-delimiter
                      'discourse-article-replies-delimiter-face))
          (reply-sep (discourse-article--make-separator
                      discourse-article-inter-reply-delimiter
                      'discourse-article-replies-delimiter-face)))
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^-- \n[*]Previous Replies[*]" nil t)
            (replace-match
             (concat start-sep
                     "\n"
                     (propertize "Previous Replies" 'face 'discourse-article-replies-heading-face)
                     "\n"
                     start-sep
                     "\n"))
            (while (re-search-forward "^\\(Posted by \\([^ ]+\\) .*\\)$" nil t)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (put-text-property beg end 'face 'discourse-article-reply-author-face)
                )
              (insert (concat "\n" reply-sep "\n")))))))))

(defun discourse-article--make-separator (delim face)
  (propertize (discourse-article--make-separator-raw delim) 'face face))

;; Adapted from `gnus-article-treat-body-boundary'.
(defun discourse-article--make-separator-raw (delim)
  (when (> (length delim) 0)
    (let ((max (window-width))
          str)
      (while (>= max (length str))
	(setq str (concat str delim)))
      (substring str 0 max))))

(defcustom discourse-article-treat-previous-replies nil
  "Treat the previous replies included in the Discourse e-mail.

No need to enable this treatment if your Discourse settings don't include
previous replies at the bottom of each e-mail.

Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'discourse-article
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-article-treat-previous-replies 'highlight t)

(defun discourse-article--find-quote ()
  (when (re-search-forward "^\\[quote=\"\\([^\"]*\\)\"\\]" nil t)
    (let ((beg-opening (match-beginning 0))
          (end-opening  (match-end 0))
          (meta (match-string 1)))
      (when (re-search-forward "\\[/quote\\]")
        (let* ((beg-closing (match-beginning 0))
               (end-closing (match-end 0))
               (text (buffer-substring end-opening beg-closing)))
          `(,beg-opening ,end-closing ,meta ,text))))))

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
          (catch 'done
            (while (< (point) (point-max))
              (-if-let ((beg end meta text) (discourse-article--find-quote))
                  (let* ((author (progn (string-match "^\\([^,]+\\)," meta)
                                        (match-string 1 meta)))
                         (attrib (concat author " wrote:"))
                         (new-text (with-temp-buffer
                                     (save-excursion (insert text))
                                     (let ((fill-column article-fill-column))
                                       (fill-region (point-min) (point-max)))
                                     (save-excursion
                                       (while (< (point) (point-max))
                                         (insert "> ")
                                         (forward-line)))
                                     ;; Discourse quotes may contain HTML entities.
                                     (or (ignore-errors
                                           (xml-parse-string))
                                         (buffer-string)))))
                    (delete-region beg end)
                    (goto-char beg)
                    (insert
                     (concat (propertize attrib 'face '(:inherit gnus-cite-1 :slant italic))
                             "\n"
                             (propertize new-text 'face 'gnus-cite-1)
                             "\n")))
                (throw 'done nil)))))))))

(defcustom discourse-article-treat-quotes t
  "Replace Discourse-style quotes with mail-style ones.
Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'discourse-article
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-article-treat-quotes 'highlight t)

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
          (while (< (point) (point-max))
            (if (looking-at "^[ \t]*```")
                (progn
                  (put-text-property (point) (1+ (point)) 'fenced-code-block t)
                  (setq in-code-block (not in-code-block))
                  (if in-code-block
                      (when (not (looking-back "\n\n"))
                        (insert "\n"))
                    (when (not (looking-at "^[ \t]*```\n\n"))
                      (end-of-line)
                      (insert "\n"))))
              (if in-code-block
                  (progn
                    (put-text-property (point) (1+ (point)) 'fenced-code-block t)
                    (when discourse-article-code-background
                      (put-text-property (point) (1+ (line-end-position))
                                         'face 'discourse-article-code-background-face)))
                ;; Preformatted block? (indented 4 spaces)
                (when (looking-at "^    ")
                  (put-text-property (point) (1+ (point)) 'preformatted-block t))))
            (forward-line)))))))

(defun discourse-article-transform-links ()
  "Make Discourse links clickable.

A Discourse link has the form:

    [LABEL](URL)

This function replaces such a link with: LABEL, where LABEL is
highlighted with the `link' face and when clicked, follows the
link to URL.

As a consequence, the paragraph may need to be re-filled. Hence
this treatment should be applied before
`discourse-article-fill-paragraphs'."
       (interactive)
       (when (discourse-article--is-discourse)
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
                   (insert (propertize cleaned-label 'face 'link 'url url))
                   ;; FIXME: Fix "widget-tabable-at: Wrong type argument: consp, #<overlay ...>"
                   ;; when TAB'ing to the button and clicking on it (RET does work, though).
                   ;; Apparently this is raised by `widget-tabable-at' when it calls
                   ;; `(widget-get widget :tab-order)`.
                   (make-button beg (point) 'face 'link
                                'help-echo (concat "Go to " url)
                                'url url
                                'action 'discourse-article--follow-link))))))))

(defun discourse-article--follow-link (ovl)
  (let ((url (overlay-get ovl 'url)))
    (gnus-button-embedded-url url)))

(defun discourse-article-transform-users ()
  "Make user names clickable."
  (interactive)
  (when (discourse-article--is-discourse)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "@\\([a-zA-Z0-9._-]+[a-zA-Z0-9_-]\\)" nil t)
          ;; Only apply the transformation outside fenced code blocks.
          (when (not (get-text-property (line-beginning-position) 'fenced-code-block))
            (let* ((beg (match-beginning 0))
                   (user (match-string 0))
                   (user-name (match-string 1))
                   (url (concat "https://users.rust-lang.org/u/" user-name)))
              (make-button beg (point) 'face 'discourse-article-user-face
                           'help-echo (concat "Go to profile of user " user-name)
                           'url url
                           'action 'discourse-article--follow-link))))))))

(defun discourse-article-highlight-sections ()
  "Highlight the section titles."
  (when (discourse-article--is-discourse)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\) \\(.*\\)$" nil t)
          (let* ((beg (match-beginning 0))
                 (end (match-end 0))
                 (title (match-string 2))
                 (level (min 6 (length (match-string 1))))
                 (face (intern (format "discourse-article-section-face-%d" level))))
            (delete-region beg end)
            (insert (propertize title 'face face))))))))

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
          (cond
           ((looking-at "^[ \t]*$") (forward-line))
           ((get-text-property (point) 'fenced-code-block) (forward-paragraph))
           ((get-text-property (point) 'preformatted-block) (forward-paragraph))
           ((< 0 (discourse-article--count-citation-marks)) (forward-paragraph))
           (t (fill-paragraph) (forward-paragraph))))))))

(defun discourse-article-transform-paragraphs ()
  "Transform an e-mail's paragraphs.
Applies `discourse-article-space-out-code-blocks',
`discourse-article-transform-links' and
`discourse-article-fill-paragraphs', in that order.
Refer to the doc of these functions for details."
  (interactive)
  (discourse-article-space-out-code-blocks)
  (discourse-article-highlight-sections)
  (discourse-article-transform-links)
  (discourse-article-transform-users)
  (discourse-article-fill-paragraphs))

(defcustom discourse-article-treat-paragraphs nil
  "Fill-wrap paragraphs except the fenced code block ones and the citations.

This is *experimental* and may break paragraph formatting, in particular if
they contain non-fenced code, so it's off by default.

Note that when enabled, the treatment is only performed on e-mails detected as
coming from a Discourse forum (see `discourse-article--is-discourse').

Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'discourse-article
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-article-treat-paragraphs 'highlight t)

;; IMPORTANT:
;;
;; - The Discourse treatments *must* appear in the following order: transform
;;   previous replies, transform quotes, and then transform paragraphs.
;;
;; - These treatments *must* also appear *before* the `nice-citation'
;;   treatment, if present (as otherwise, `nice-citation' would run before the
;;   Discourse quotes treatment and thus would leave these quotes
;;   un-prettified).
(nconc gnus-treatment-function-alist
       '((discourse-article-treat-previous-replies discourse-article-transform-previous-replies)
         (discourse-article-treat-quotes discourse-article-transform-quotes)
         (discourse-article-treat-paragraphs discourse-article-transform-paragraphs)))

(provide 'discourse-article)
;;; discourse-article.el ends here

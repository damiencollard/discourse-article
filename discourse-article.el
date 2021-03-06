;;; discourse-article.el --- Treatments for Discourse e-mails

;; Copyright (c) 2019 Damien Collard

;; Author: Damien Collard <damien.collard@laposte.net>
;; URL:
;; Version: 0.0.3
;; Keywords: gnus mail convenience
;; Package-Requires: ((emacs "24.3") (dash "2.12.0") (gnus "5.13") (markdown-mode "2.3"))

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
(require 'markdown-mode)

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

(defcustom discourse-article-fill-paragraphs nil
  "Whether to fill paragraphs.
Paragraphs are filled to the column specified by `fill-column`.
This is disabled by default as currently it will mess up itemized
lists."
  :group 'discourse-article
  :type 'boolean)

(defcustom discourse-article-highlight-code-blocks t
  "Whether to apply syntax highlighting to the code blocks.
When enabled, the highlighting is performed via `markdown-mode'."
  :group 'discourse-article
  :type 'boolean)

(defcustom discourse-article-replies-beginning-delimiter "━"
  "String used to mark the beginning of previous replies."
  :group 'discourse-article
  :type '(choice (item :tag "None" :value nil)
		 string))

(defcustom discourse-article-inter-reply-delimiter "─"
  "String used to separate replies."
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

;; (defface discourse-article-code-background-face
;;   '((t (:background "#2e2e2e")))
;;   "Face for the background of fenced code blocks."
;;   :group 'discourse-article)

;; (defcustom discourse-article-code-background t
;;   "Whether to color the background of fenced code blocks.
;; The face is `discourse-article-code-background-face'."
;;   :group 'discourse-article
;;   :type 'boolean)

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

(defun discourse-article--make-code-block-separator (&optional lang)
  (let ((label (if (> (length lang) 0)
                   (concat " " lang " ")
                 "")))
    (concat (propertize (make-string 3 ?•) 'face '(:foreground "#464646"))
            (propertize label 'face '(:foreground "#868686"))
            (propertize (make-string (- fill-column 3 (length label)) ?•) 'face '(:foreground "#464646")))))

(defun discourse-article--make-code-block-separator (&optional lang)
  (let ((label (if (> (length lang) 0)
                   (concat " " lang " ")
                 "")))
    (concat (propertize (make-string 3 ?•) 'face '(:foreground "#464646"))
            (propertize label 'face '(:foreground "#868686"))
            (propertize (make-string (- fill-column 3 (length label)) ?•) 'face '(:foreground "#464646")))))

(defun discourse-article-transform-code-blocks ()
  "Ensure fenced code blocks are separate paragraphs.
Fenced code blocks are delimited by triple backticks.
A newline is inserted before and after, if needed."
  (interactive)
  (when (discourse-article--is-discourse)
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (let (lang block-beg)
          (while (< (point) (point-max))
            (if block-beg ;; Inside a block?
                (progn
                  (put-text-property (point) (1+ (point)) 'fenced-code-block t)
                  ;; (when discourse-article-code-background
                  ;;   (put-text-property (point) (1+ (line-end-position))
                  ;;                      'face 'discourse-article-code-background-face))
                  ;; At end of block?
                  (when (looking-at "^[ \t]*```")
                    (when discourse-article-highlight-code-blocks
                      (put-text-property (match-beginning 0) (match-end 0)
                                       'display (discourse-article--make-code-block-separator))
                      (markdown-fontify-code-block-natively lang block-beg (point)))
                    (setq block-beg nil)
                    (when (not (looking-at "^[ \t]*```\n\n"))
                      (end-of-line)
                      (insert "\n"))))
              ;; At beginning of fenced code bloc?
              (if (looking-at "^[ \t]*```[ \t]*\\([^[:space:]]*\\)")
                  (progn
                    (setq lang (match-string 1))
                    (when discourse-article-highlight-code-blocks
                      (put-text-property (match-beginning 0) (match-end 0)
                                         'display (discourse-article--make-code-block-separator lang)))
                    (put-text-property (point) (1+ (point)) 'fenced-code-block t)
                    (when (not (looking-back "\n\n"))
                      (insert "\n"))
                    (setq block-beg (save-excursion (forward-line) (line-beginning-position))))
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
`discourse-article-do-fill-paragraphs'."
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

;; XXX: This should use the `Message-ID` header to find out the base URL of the
;; Discourse forum a mail originates from, but I haven't found an easy way to
;; access this header when not all headers are displayed (which is the default).
;;
;; So instead the trick is to find the very last link in the e-mail, which is meant
;; for unsubscription and does contain the base URL. It's brittle but it works...
(defun discourse-article--forum-url ()
  "Return the URL of the Discourse forum an e-mail originates from."
  (gnus-with-article-buffer
    (save-excursion
      (goto-char (point-max))
      (let ((limit (save-excursion
                     (forward-line -1)
                     (line-beginning-position))))
        (when (looking-back "(\\(https://[^/]+\\)/.+).[[:space:]]*" limit)
          (match-string 1))))))

(defun discourse-article-transform-users (forum-url)
  "Make user names clickable.
The URL to the user profile is constructed based on FORUM-URL."
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
                   (url (concat forum-url "/u/" user-name)))
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

(defun discourse-article-do-fill-paragraphs ()
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
  "Apply several transformations to an e-mail's paragraphs."
  (interactive)
  (let ((forum-url (discourse-article--forum-url)))
    (discourse-article-transform-code-blocks)
    (discourse-article-highlight-sections)
    (discourse-article-transform-links)
    (discourse-article-transform-users forum-url)
    (when discourse-article-fill-paragraphs
      (discourse-article-do-fill-paragraphs))))

(defun discourse-article-transform-article ()
  ;; The transformations *must* appear in the following order: transform
  ;; previous replies, transform quotes, and then transform paragraphs.
  (discourse-article-transform-previous-replies)
  (discourse-article-transform-quotes)
  (discourse-article-transform-paragraphs))

(defcustom discourse-article-treat-articles nil
  "Treat articles coming from Discourse forums.

The treatment transforms Discourse's BB-style quotes into regular
mail citation form, prettifies existing links, adds links to user
profiles on @user mentions, and highlights code blocks.

The treatment is customizable with `M-x customize-group
discourse-article`.

Note that when enabled, the treatment is only performed on
e-mails detected as coming from a Discourse forum (see
`discourse-article--is-discourse').

Valid values are nil, t, `head', `first', `last', an integer or a
predicate.  See Info node `(gnus)Customizing Articles' for details."
  :group 'discourse-article
  :group 'gnus-article-treat
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)
(put 'discourse-article-treat-articles 'highlight t)

;; IMPORTANT: This treatment *must* appear *before* the `nice-citation'
;; treatment, if present (as otherwise, `nice-citation' would run before the
;; Discourse quotes treatment and thus would leave these quotes un-prettified).
(nconc gnus-treatment-function-alist
       '((discourse-article-treat-articles discourse-article-transform-article)))

(provide 'discourse-article)
;;; discourse-article.el ends here

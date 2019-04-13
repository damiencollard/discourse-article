# Discourse  articles for Gnus

Treatment of Discourse e-mails:

- Transform BB quotes into mail citations.
- Syntax highlight fenced code blocks.
+ Fill non-code paragraphs.
- Prettify links.
- Add user profile links to `@user` mentions.
- Prettify the Previous Replies section.

Here's how citations and the previous replies section look:

Before | After
:-----:|:--------------:
![Before](images/discourse-article-before.png?raw=true "Before") | ![After](images/discourse-article-after.png?raw=true "After")

It looks even better combined with [Nice Citation](https://github.com/damiencollard/nice-citation): [screenshot](images/discourse-article-after-with-nice-citation.png).

And here's how fenced code blocks look:

Before | After
:-----:|:-------------:
![Before](images/code-block-before.png?raw=true "Before") | ![After](images/code-block-after.png?raw=true "After")

**Note**: The default highlighting assumes a dark theme. If your theme is
light, you'll certainly need to customize the faces in group
`discourse-article`.

## Treatment

`discourse-article` only applies its treatments to articles coming from
Discourse forums. This is determined using regexes in customizable
variable `discourse-article-from-regexps`: if the `From` header of an
article matches any of these regexes, the article is deemed to be a
Discourse article.

### Quotes -> citations

Discourse e-mails may contain quotes (citations) of the form

```plain
    [quote=\"AUTHOR,...\"]...TEXT...[/quote].
```

This module replaces such quotes with regular e-mail citation format
as follows:

```plain
    AUTHOR wrote:
    > ...TEXT...
    > ...TEXT...
    > ...TEXT...
```

and highlights the whole with face `gnus-cite-1`.

When used in combination with [Nice Citation](https://github.com/damiencollard/nice-citation) (to get even nicer looking
citations), the Discourse Article treatment must appear *before* the
Nice Citation treatment -- see Usage below.

### Code  blocks

Fenced code blocks, which are delimited by triple backticks (```) are
moved into their own paragraph and syntax-highlighted by markdown
mode.

Syntax highlighting can be disabled by setting
`discourse-article-highlight-code-blocks` to nil.

### Paragraph filling

Non-code, non-preformatted paragraphs are fill-wrapped to `fill-column`
columns.

This is disabled by default, because currently it will likely mess up
itemized lists. It can be enabled by setting
`discourse-article-fill-paragraphs` to t.

### Labeled links and Users

It prettifies Discourse links of the form `[label](url)` by keeping only
the label, highlighted with the `link` face and by making it
clickable. Clicking it follows the specified URL.

It also adds a link to a user's profile when it encounters a user name
in the form `@user`.

### Previous replies

If in your Discourse Emails settings you enabled to include "previous
replies" at the bottom of each e-mail you receive, then this
prettifies that section a bit.

It adds separators between replies and highlights the names of the
replies' authors. The face used to highlight the user names can be
customized with `discourse-article-user-face`.

## Installation

Copy `discourse-article.el` into a directory that appears in your
Emacs' `load-path`.

## Usage

```lisp
(require 'discourse-article)
```

In case you want to apply the [`nice-citation`](https://github.com/damiencollard/nice-citation) treatment too, require it
*after* `discourse-article`:

```lisp
(require 'discourse-article)
(require 'nice-citation)
```

## Customization

`M-x customize-group discourse-article`.

## TODO

- Until the `upload://` links in plain text are fixed [^1], write hack to
  replace the `upload://` URLs with the actual URLs to the asset

[^1]: Bug https://meta.discourse.org/t/mailing-list-mode-upload-links-broken-in-e-mails/112354

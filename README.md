# Discourse  articles for Gnus

Treatment of Discourse e-mails.

Discourse e-mails may contain quotes (citations) of the form

```plain
    [quote=\"AUTHOR,...\"]...TEXT...[/quote].
```

This module adds a treatment function that replaces such quotes with
regular e-mail citation format as follows:

```plain
    AUTHOR wrote:
    > ...TEXT...
    > ...TEXT...
    > ...TEXT...
```

and highlights the whole with face `gnus-cite-1`.

It can be used in combination with
[nice-citation](https://github.com/damiencollard/nice-citation) to get
even nicer looking citations. In that case, the Discourse Article treatment must appear *before* the Nice Citation treatment -- this Usage below

## Installation

Copy `discourse-article.el` into a directory that appears in your
Emacs' `load-path`.

## Usage

```lisp
(require 'discourse-article)
```

In case you want to apply the [nice-citation](https://github.com/damiencollard/nice-citation) treatment too, require it *after* `discourse-article`:

```lisp
(require 'discourse-article)
(require 'nice-citation)
```

## Customization

The Discourse treatment can be disabled in article mode by setting
`discourse-article-treat-quotes` in group `gnus-article-treat` to nil.

## TODO

- Fill-wrap paragraphs other than quotes -- use `gnus-article-fill-long-lines`?
- Do not fill-wrap code blocks inside quotes!
- Highlighting of code blocks (with `polymode`?).

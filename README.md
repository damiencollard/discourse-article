# Discourse  articles for Gnus

Treatment of Discourse e-mails. There are actually 2 independent treatments:

- Quotes
- Code + paragraphs.

## Quotes -> citations

This treatment is *enabled* by default.

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

This treatment only applies to Discourse e-mails (based on the `From`
header containing `@discoursemail.com`).

## Code + paragraphs

This treatment is *disabled* by default.

Fenced code blocks, which are delimited by triple backticks (```) are moved
into separate paragraphs, so as to ease the next treatment: wrapping
of paragraphs.

Non-code paragraphs are fill-wrapped to `fill-column` columns.

**NOTE**: Code blocks are not highlighted yet.

**NOTE**: If a paragraph contains (non-fenced) code blocks, this will
likely garble the code, hence its being disabled by default.

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

- The quotes treatment can be disabled by setting `discourse-article-treat-quotes` to nil.
- The code + paragraphs treatment can be enabled by setting `discourse-article-treat-paragraphs` to t.

Both settings are in group `gnus-article-treat`.

## TODO

- Highlighting of code blocks (with `polymode`?).
- Buttonification of labeled links (of the form `[label](url)`).

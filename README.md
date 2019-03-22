# Discourse  articles for Gnus

Treatment of Discourse e-mails. There are actually 2 independent treatments:

- Quotes
- Code + Paragraphs
- Labeled Links
- Previous Replies.

Here's how it looks (combined with [nice-citation](https://github.com/damiencollard/nice-citation)):

![Prettified Discourse article](images/discourse-article.png?raw=true "Prettified Discourse article")

## Treatments

### Quotes -> citations

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

### Code + paragraphs

This treatment is *disabled* by default.

Fenced code blocks, which are delimited by triple backticks (```) are moved
into separate paragraphs, so as to ease the next treatment: wrapping
of paragraphs.

Non-code paragraphs are fill-wrapped to `fill-column` columns.

**NOTE**: Code blocks are not highlighted yet.

**NOTE**: If a paragraph contains (non-fenced) code blocks, this will
likely garble the code, hence its being disabled by default.

### Labeled links

This treatment is *enabled* by default.

The Labeled Links treatment transforms Discourse links of the form
`[label](url)` by keeping only the label highlighted with the `link` face
and making it clickable. Clicking it follows the specified URL.

### Previous replies

This treatment is *enabled* by default.

If in your Discourse Emails settings you enabled to include previous
replies at the bottom of each e-mail you receive, then this treatment,
if not perfect, improves readability quite a bit.

It adds separators between replies and highlights the names of the
replies' authors.

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

- The Quotes treatment can be disabled by setting `discourse-article-treat-quotes` to nil.
- The Code + Paragraphs treatment can be enabled by setting `discourse-article-treat-paragraphs` to t.
- The Previous Replies treatment can be disabled by setting `discourse-article-treat-previous-replies` to nil.

- The delimiter strings, the faces for the delimiters, the replies
  heading and the replies' authors can be customized.

All these settings are in group `discourse-article`. The treatment variables are in
`gnus-article-treat` as well.

## TODO

- Highlighting of code blocks (with `polymode`?).

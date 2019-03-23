# Discourse  articles for Gnus

Treatment of Discourse e-mails. There are actually 2 independent treatments:

- Quotes
- Code + Paragraphs
- Labeled Links
- Previous Replies.

Here's how it looks:

Standard | With treatment
:-------:|:-----------------:
![Standard](images/discourse-article-before.png?raw=true "Standard")  | ![After](images/discourse-article-after.png?raw=true "With discourse-article")

And it looks even better combined with [Nice Citation](https://github.com/damiencollard/nice-citation): [screenshot](images/discourse-article-after-with-nice-citation.png).

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

This treatment only applies to Discourse e-mails (based on the `From`
header containing `@discoursemail.com`).

When used in combination with
[Nice Citation](https://github.com/damiencollard/nice-citation) (to
get even nicer looking citations), the Discourse Article treatment
must appear *before* the Nice Citation treatment -- see Usage below.

### Code + paragraphs

This treatment is *disabled* by default.

Fenced code blocks, which are delimited by triple backticks (```) are moved
into separate paragraphs, so as to ease the next treatment: wrapping
of paragraphs.

This treatment also detects preformatted blocks, which are paragraphs
whose lines all start with 4 spaces. Note that *paragraph* implies a
blank line before and after -- if that's not the case, it won't be
considered a preformatted block.

Non-code, non-preformatted paragraphs are fill-wrapped to
`fill-column` columns.

**NOTE**: Code blocks are not highlighted yet.

**NOTE**: If a paragraph contains itemized lists, this will likely
garble them, hence its being disabled by default.

### Labeled links

This treatment is *enabled* by default.

The Labeled Links treatment transforms Discourse links of the form
`[label](url)` by keeping only the label highlighted with the `link` face
and making it clickable. Clicking it follows the specified URL.

### Previous replies

This treatment is *disabled* by default.

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

In case you want to apply the [`nice-citation`](https://github.com/damiencollard/nice-citation)
treatment too, require it *after* `discourse-article`:

```lisp
(require 'discourse-article)
(require 'nice-citation)
```

## Customization

- The Quotes treatment can be disabled by setting `discourse-article-treat-quotes` to nil.
- The Code + Paragraphs treatment can be enabled by setting `discourse-article-treat-paragraphs` to t.
- The Previous Replies treatment can be enabled by setting `discourse-article-treat-previous-replies` to t.

- The fenced code block's background can be customized.
- The "Previous Replies" delimiter strings, the faces for the
  delimiters, the replies heading and the replies' authors can be
  customized.

All these settings are in group `discourse-article`. The treatment variables are in
`gnus-article-treat` as well.

## TODO

- Highlighting of code blocks (with `polymode`?).

# Discourse  articles for Gnus

Treatment of Discourse e-mails. There are actually 3 independent treatments:

- Quotes
- Code + Paragraphs + Labeled Links + Users
- Previous Replies.

Here's how it looks:

Standard | With treatment
:-------:|:-----------------:
![Standard](images/discourse-article-before.png?raw=true "Standard")  | ![After](images/discourse-article-after.png?raw=true "With discourse-article")

And it looks even better combined with [Nice Citation](https://github.com/damiencollard/nice-citation): [screenshot](images/discourse-article-after-with-nice-citation.png).

**Note**: The default highlighting assumes a dark theme. If your theme is
light, you'll certainly need to customize the faces in group
`discourse-article`.

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
header matching one of the customizable regexes in
`discourse-article-from-regexps`, see Customization further down).

When used in combination with [Nice Citation](https://github.com/damiencollard/nice-citation) (to get even nicer looking
citations), the Discourse Article treatment must appear *before* the
Nice Citation treatment -- see Usage below.

### Code + paragraphs

This treatment is *disabled* by default.

Fenced code blocks, which are delimited by triple backticks (```) are
moved into separate paragraphs and syntax-highlighted by default.

This treatment also detects preformatted blocks, which are paragraphs
whose lines all start with 4 spaces. Note that *paragraph* implies a
blank line before and after -- if that's not the case, it won't be
considered a preformatted block.

Non-code, non-preformatted paragraphs are fill-wrapped to
`fill-column` columns.

**NOTE**: Code blocks are not highlighted yet.

**NOTE**: If a paragraph contains itemized lists, this will likely
garble them, hence its being disabled by default.

### Labeled links and Users

This treatment is *enabled* by default.

It transforms Discourse links of the form `[label](url)` by keeping only
the label highlighted with the `link` face and making it
clickable. Clicking it follows the specified URL.

It also adds a link to a user's profile when it encounters a user name
in the form `@user`.

### Previous replies

This treatment is *disabled* by default.

If in your Discourse Emails settings you enabled to include previous
replies at the bottom of each e-mail you receive, then this treatment,
if not perfect, improves readability quite a bit.

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

In case you want to apply the [`nice-citation`](https://github.com/damiencollard/nice-citation)
treatment too, require it *after* `discourse-article`:

```lisp
(require 'discourse-article)
(require 'nice-citation)
```

## Customization

- `discourse-article` only applies its treatments to articles coming
  from Discourse forums. This is determined using regexes in
  customizable variable `discourse-article-from-regexps`: if the `From`
  header of an article matches any of these regexes, the article is
  deemed to be a Discourse article.

- The Quotes treatment can be disabled by setting `discourse-article-treat-quotes` to nil.
- The Code + Paragraphs treatment can be enabled by setting `discourse-article-treat-paragraphs` to t.
- The Previous Replies treatment can be enabled by setting `discourse-article-treat-previous-replies` to t.

- The syntax highlighting of code blocks can be disabled.
- ~~The fenced code block's background can be customized.~~ Temporarily
  disabled until its combination with syntax highlighting is resolved.
- The "Previous Replies" delimiter strings, the faces for the
  delimiters, the replies heading and the replies' authors can be
  customized.

All these settings are in group `discourse-article`. The treatment
variables are present in `gnus-article-treat` as well.

## TODO

- Until the `upload://` links in plain text are fixed [^1], write hack to
  replace the `upload://` URLs with the actual URLs to the asset

[^1]: Bug https://meta.discourse.org/t/mailing-list-mode-upload-links-broken-in-e-mails/112354

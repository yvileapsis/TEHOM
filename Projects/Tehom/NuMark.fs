namespace Tehom

open System
open Nu

(*
    let textTest  =
        """---
__Advertisement :)__

- __[pica](https://nodeca.github.io/pica/demo/)__ - high quality and fast image
  resize in browser.
- __[babelfish](https://github.com/nodeca/babelfish/)__ - developer friendly
  i18n with plurals support and easy syntax.

You will like those projects!

---

# h1 Heading 8-)
## h2 Heading
### h3 Heading
#### h4 Heading
##### h5 Heading
###### h6 Heading


## Horizontal Rules

___

---

***


## Typographic replacements

Enable typographer option to see result.

(c) (C) (r) (R) (tm) (TM) (p) (P) +-

test.. test... test..... test?..... test!....

!!!!!! ???? ,,  -- ---

"Smartypants, double quotes" and 'single quotes'


## Emphasis

**This is bold text**

__This is bold text__

*This is italic text*

_This is italic text_

~~Strikethrough~~


## Blockquotes


> Blockquotes can also be nested...
>> ...by using additional greater-than signs right next to each other...
> > > ...or with spaces between arrows.


## Lists

Unordered

+ Create a list by starting a line with `+`, `-`, or `*`
+ Sub-lists are made by indenting 2 spaces:
  - Marker character change forces new list start:
    * Ac tristique libero volutpat at
    + Facilisis in pretium nisl aliquet
    - Nulla volutpat aliquam velit
+ Very easy!

Ordered

1. Lorem ipsum dolor sit amet
2. Consectetur adipiscing elit
3. Integer molestie lorem at massa


1. You can use sequential numbers...
1. ...or keep all the numbers as `1.`

Start numbering with offset:

57. foo
1. bar


## Code

Inline `code`

Indented code

    // Some comments
    line 1 of code
    line 2 of code
    line 3 of code


Block code "fences"

```
Sample text here...
```

Syntax highlighting

``` js
var foo = function (bar) {
  return bar++;
};

console.log(foo(5));
```

## Tables

| Option | Description |
| ------ | ----------- |
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |

Right aligned columns

| Option | Description |
| ------:| -----------:|
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |


## Links

[link text](http://dev.nodeca.com)

[link with title](http://nodeca.github.io/pica/demo/ "title text!")

Autoconverted link https://github.com/nodeca/pica (enable linkify to see)


## Images

![Minion](https://octodex.github.com/images/minion.png)
![Stormtroopocat](https://octodex.github.com/images/stormtroopocat.jpg "The Stormtroopocat")

Like links, Images also have a footnote style syntax

![Alt text][id]

With a reference later in the document defining the URL location:

[id]: https://octodex.github.com/images/dojocat.jpg  "The Dojocat"


## Plugins

The killer feature of `markdown-it` is very effective support of
[syntax plugins](https://www.npmjs.org/browse/keyword/markdown-it-plugin).


### [Emojies](https://github.com/markdown-it/markdown-it-emoji)

> Classic markup: :wink: :cry: :laughing: :yum:
>
> Shortcuts (emoticons): :-) :-( 8-) ;)

see [how to change output](https://github.com/markdown-it/markdown-it-emoji#change-output) with twemoji.


### [Subscript](https://github.com/markdown-it/markdown-it-sub) / [Superscript](https://github.com/markdown-it/markdown-it-sup)

- 19^th^
- H~2~O


### [\<ins>](https://github.com/markdown-it/markdown-it-ins)

++Inserted text++


### [\<mark>](https://github.com/markdown-it/markdown-it-mark)

==Marked text==


### [Footnotes](https://github.com/markdown-it/markdown-it-footnote)

Footnote 1 link[^first].

Footnote 2 link[^second].

Inline footnote^[Text of inline footnote] definition.

Duplicated footnote reference[^second].

[^first]: Footnote **can have markup**

    and multiple paragraphs.

[^second]: Footnote text.


### [Definition lists](https://github.com/markdown-it/markdown-it-deflist)

Term 1

:   Definition 1
with lazy continuation.

Term 2 with *inline markup*

:   Definition 2

        { some code, part of Definition 2 }

    Third paragraph of definition 2.

_Compact style:_

Term 1
  ~ Definition 1

Term 2
  ~ Definition 2a
  ~ Definition 2b


### [Abbreviations](https://github.com/markdown-it/markdown-it-abbr)

This is HTML abbreviation example.

It converts "HTML", but keep intact partial entries like "xxxHTMLyyy" and so on.

*[HTML]: Hyper Text Markup Language

### [Custom containers](https://github.com/markdown-it/markdown-it-container)

::: warning
*here be dragons*
:::"""



    runParserOnString parseText UserState.Default "" """~~value~~ {whatever}~test~
    | testing a second line |
    whatever |
"""

    runParserOnString parseText UserState.Default "" """{test}~~**This is bold text** __This is underlined text__~~
{also a style}*This is italic text*  |
_This is subscript text_ |
~~Strikethrough~~ |
~This is Normal Text~ |
"""


//    run parseLine "**bold ~~bold and strikethrough~~** normal *italics* ~*italics superscript*~"
    runParserOnString (parseLine (followedBy eof)) UserState.Default "" "**bold*italics*bold2**"

    runParserOnString (parseLine (followedBy eof)) UserState.Default "" "__bold\\\*\_bold2__*"
    runParserOnString (parseLine (followedBy eof)) UserState.Default "" "**bold*bold2**"

    runParserOnString (parseLine (followedBy eof)) UserState.Default "" "**bold ~~strike through~~ bold2**"
    runParserOnString (parseLine (followedBy eof)) UserState.Default "" "**bold~~strike through~~bold2**"
    *)(*
    This is a Markdown implementation based on Github flavored Markdown, but with quite a few alterations for easier
    writing and comprehension. The main way of formatting text is through the use of special characters around formatted
    text. Additionally, a form of tagging is supported.

    What we're going to leverage for this markup language is that majority of formating is done on per-line basis.

    Decisions and limitations:

    1. Markdown formatting:
        - Formatting blocks are contained within a string of special chars.
        - Unlike Github flavored Markdown tags don't need to touch words, more similar to Discord's Markdown. This
          serves two purposes: easier typing and easier parsing.
        - Nesting is not supported for the same tag, as it wouldn't provide any additional sense.
        - Nesting is supported for different tags.
        - Interlinked nesting isn't supported.

    2. Additional tags formatting:
        - Contained within "<tag>" and "</tag>"
        - If tag is not closed entirety of the line till the end is considered to be in the tag.

    3. Unlike Github Markdown I do not intend to support nested subscript and superscript, as those see very limited use
       and it's hard to imagine game context where that would work as well.


    *)

    // Have to define local justification types as
    // 1. there is a justification type not seen there, the full line justification
    // 2. there is no such thing as unjustified text in rich text block



(*
    White space before, in the middle, etc of commands is ignored

    ### Header
    # # # Same tier of header
      ### Also the same header with the same indentation

    >>> triple quote
    > > > also triple quote

    Right quote <<<

    ### Centered Header ###

    Header To The Right ###   <- there could be unlimited whitespace but not any symbols

    | Normal Paragraph
    Also Normal Paragraph
    | Centered Paragraph |
    Right Oriented Paragraph |

    ``` ``` <- code block
    ` ` <- one line code
    |: Table with one |
    | - |

    | Table | With :|: Several |
    | - | - | - |

    classic table

    | Whatever | Whatever | whatever |
    |:-|:-:|-:|

*)
namespace Tehom

module NuMark =

    #r "nuget: FParsec"
    open FParsec
    (*

    Regular Markdown has very limited styling capabilities and is often paired with bbcode/html/ansi code to fix it.
    The result is not very cohesive and quite annoying to write.

    What we're going to leverage for this markup language is that majority of formating is done on per-line basis.
    *)

    type Formatted = {
        Size: string
        Style: string
        Color: string
    }
    with
        static member empty = {
            Size = ""
            Style = ""
            Color = ""
        }

    let parseFormat specialChars : Parser<Formatted, unit> =

        many1Chars (noneOf specialChars) |>> (fun x -> { Formatted.empty with Size = x})


    type Justification = Justification

    type Line =
        | String of string
        | List of Line list
        | Normal of Line
        | Bold of Line
        | Italics of Line
        | Strikethrough of Line
        | Underlined of Line
        | Subscript of Line
        | Superscript of Line
        | Formatted of Formatted * Line

    let parseLine : Parser<Line list, unit> =
        let lineElement, lineElementRef = createParserForwardedToRef()

        let text specialChars = many1Satisfy (isNoneOf specialChars) |>> Line.String <?> "string"
        let trailingSymbol specialChars = many1Satisfy (isAnyOf specialChars) |>> Line.String <?> "trailing"

        let listToSymbol symbol = many1Till lineElement (followedByString symbol)

        let style styleType symbol =
            pstring symbol
            >>. listToSymbol symbol
            .>> pstring symbol
            |>> function [ oneMember ] -> oneMember | list -> List list
            |>> styleType
            <?> "style"

        let allStyles = choice [
            style Bold "**"
            style Italics "*"
            style Strikethrough "~~"
            style Normal "~"
            style Underlined "__"
            style Subscript "_"
            style Superscript "^"
        ]

        let color =
            pstring "{"
            >>. parseFormat "{}"
            .>> pstring "}"
            .>>. allStyles
            |>> Formatted
            <?> "color"

        lineElementRef.Value <- choice [
            text "~^*_{}"
            attempt color
            attempt allStyles
            trailingSymbol "~^*_{}"
        ]

        many1 lineElement


    run parseLine """~~value~~ {tseta} ~~whatever"""

    run parseLine """{test}~~**This is bold text** __This is underlined text__~~ {also a style}*This is italic text* _This is subscript text_ ~~Strikethrough~~ ~This is Normal Text~"""


    type Node =
        | Header of Node
        | Quote of Node
        | Indent of Node
        | List of Node
        | Code
        | Codeblock
        | Table
        | Image
        | HorizontalLine
        | Line of Justification * Line


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
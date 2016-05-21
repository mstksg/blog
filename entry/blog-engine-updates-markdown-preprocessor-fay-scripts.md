Blog engine updates: Markdown Preprocessor & Fay Scripts
========================================================

[Read online!](http://home.jle0.com:4111/entry/blog-engine-updates-markdown-preprocessor-fay-scripts.html)

*Posted by [Justin Le](http://home.jle0.com:4111/) on January 27, 2014*

I spent some time over the past week writing a preprocessor for the
entry copy markdowns and getting Fay to deploy some simple scripts.

The need for a preprocessor was sparked by a post I’m writing that sort
of necessitated the features. I write [all of my
posts](https://github.com/mstksg/inCode/tree/master/copy/entries) in
markdown, and it all integrated well with the preprocessor. In addition
I needed some javascript scripting to make the preprocessor actions
worthwhile, so I buckled down and wrestled with getting Fay to work in a
production environment. So I guess this post is to show off some new
features of the blog engine?

Here it is in action:

    > !!!monad-plus/WolfGoatCabbage.hs "makeMove ::" "moveLegal ::" wolf-goat-cabbage

yields:

``` {.haskell}
-- source: https://github.com/mstksg/blog/tree/develop/code-samples/monad-plus/WolfGoatCabbage.hs#L37-58
-- interactive: https://www.fpcomplete.com/user/jle/wolf-goat-cabbage
makeMove :: Plan -> [Plan]
makeMove p = do
    next <- MoveThe <$> [Farmer ..]
    guard       $ moveLegal p next
    guard . not $ moveRedundant p next
    let
        p' = p ++ [next]
    guard $ safePlan p'
    return p'

moveLegal :: Plan -> Move -> Bool
moveLegal p (MoveThe Farmer)  = True
moveLegal p (MoveThe c)       = positionOf p c == positionOf p Farmer

```

(Mouse over or click them to see the full effect :) )

Code quoting/linking preprocessor
---------------------------------

So I find myself writing a lot of sample code for my posts, and then
later copying and pasting them over to the
[code-samples](https://github.com/mstksg/inCode/tree/master/code-samples)
directory on the github in order to allow people to download them…and
then later awkwardly putting a link saying “download these here!”
afterwards and taking up space. Also linking to a live
[FPComplete](https://www.fpcomplete.com/) version is a bit awkward too,
right after the block.

I was rather inspired by the interface on the code blocks for [luite’s
blog](http://weblog.luite.com/wordpress/?p=127), where every relevant
code block has a little link box on the top right hand corner linking to
the source and a working/running example.

So I wrote a Haskell preprocessor to take in a specification of a code
file and a what blocks in the code file to load, and then load it into
the markdown file before it is processed by pandoc.

The syntax is:

    > !!!path/to/code "keyword" "limited"n live_link

Where “keyword” is the text in the line to match for, `n` is the number
of lines after the keyword to display (if left off, it takes the next
“block”, or the next continuous piece of code before a new non-indented
line), and `live_link` is a link to the live/interactive version on
FPComplete.

### Reflections

So…writing the parser for the syntax specification was pretty easy due
to parsec and parser combinators:

``` {.haskell}
-- source: https://github.com/mstksg/blog/tree/develop/code-samples/source/EntryPP.hs#L32-127
data SampleSpec = SampleSpec  { sSpecFile       :: FilePath
                              , _sSpecLive      :: Maybe String
                              , _sSpecKeywords  :: [(String,Maybe Int)]
                              } deriving (Show)

sampleSpec :: Parser SampleSpec
sampleSpec = do
    filePath <- noSpaces <?> "sample filePath"
    spaces
    keywords <- many $ do
      keyword <- char '"' *> manyTill anyChar (char '"') <?> "keyword"
      keylimit <- optionMaybe (read <$> many1 digit <?> "keyword limit")
      spaces
      return (keyword,keylimit)

    live <- optionMaybe noSpaces <?> "live url"
    let
      live' = mfilter (not . null) live

    return $ SampleSpec filePath live' keywords
  where
    noSpaces = manyTill anyChar (space <|> ' ' <$ eof)

```

The code to actually find the right code block to paste was complicated
and horrifying at first, but after I sat down and really sorted out the
logic, it wasn’t too bad. Still, it isn’t the cleanest code in the world
and I wonder how I could have made it better, either with a Haskell
library or even another language.

This all left two little comment lines before the source code insert
with the link to the source and interactive versions.

All that was left was a front-end script to get the comments and turn
them into floating divs.

Fay
---

Ah okay, here was the fun part.

Fay is actually pretty fun to use. And while it was perhaps complete
overkill to use the entire Fay runtime and build system for just a
simple script, but I was pretty inspired by [ocharles’s post on
fay](http://ocharles.org.uk/blog/posts/2013-12-23-24-days-of-hackage-fay.html)
and I thought this would be a good time to get to learn it.

So there was a lot of cognitive friction going in, and trying to really
get in the groove took a few days. There was also a rather unhelpful
error message involving the ffi that I was able to bring up to the
maintainers and be a part of getting the fix working.

I converted as much of my current scripts as I could to fay. There was
one that I couldn’t — a function call to a library that required a
javascript object of function callbacks, and I couldn’t really get that
to work cleanly and I decided it wasn’t worth the effort for now — maybe
another day. If anything I could re-write the entire library (a Table of
Contents generator) myself some day.

### Reflections

#### fay-jquery

Here is a characteristic example of fay code with fay-jquery (0.6.0.2):

``` {.haskell}
-- source: https://github.com/mstksg/blog/tree/develop/code-samples/source/entry.hs#L45-54
appendTopLinks :: Fay ()
appendTopLinks = do
  mainContent <- select ".main-content"
  headings <- childrenMatching "h2,h3,h4,h5" mainContent
  J.append topLink headings
  topLinks <- select ".top-link"
  click (scrollTo 400) topLinks
  return ()
  where
    topLink = "<a href='#title' class='top-link'>top</a>"

```

As you can see, some of the method calls in fay-jquery seem a bit
backwards…I keep on resisting the urge to write things like

``` {.haskell}
container `append` contained
container `childrenMatching` ".contained"
```

Which matches the JQuery calling model:

``` {.javascript}
container.append(contained);
container.children('.contained');
```

But alas, the decision was made otherwise. I guess it is more Haskell-y
to be able to play with partial application and do something like

``` {.haskell}
let
  appendIt = append container
in
  appendIt contained1
  appendIt contained2
  appendIt contained3
```

So I guess that’s okay.

However, something I was less understanding of was the ordering for
event binding, which needed the handlers *before* the object being
binded.

``` {.haskell}
-- source: https://github.com/mstksg/blog/tree/develop/code-samples/source/entry.hs#L112-119
processCodeBlocks :: Fay ()
processCodeBlocks = do
  blocks <- select ".main-content pre.sourceCode"
  flip each blocks $ \_ el -> do
    elJ <- select el
    processBlock elJ
    return True
  return ()

```

This one kind of bucks the convention that methods like `append`
maintain…and also needs those annoying `flips` to have easy anonymous
callbacks. I don’t want to have to name every little thing. Oh well.
Maybe there is a good justification here? I just don’t see it. But then
again, there is a reason why we have both `mapM` and `forM` in base.

#### Deploying fay

Deploying fay ain’t all too bad. I [deploy
binaries](http://blog.jle.im/entry/deploying-medium-to-large-haskell-apps-to-heroku),
however, so I was unable to ever process fay on my limited-access
production server because it requires `ghc-pkg` (installed under
`/usr/local/bin`) among other things…I probably could have gotten this
to work, but I did not have the proper skills. You also need to provide
the binaries and headers in `share` for all of your fay libraries in
order to use them when compiling to javascript. So while this isn’t so
bad if you have the whole Haskell Platform and are compiling on your
production server, I had to pre-compile my fay “binaries” before
pushing…just like I have to pre-compile my regular binaries,
interestingly enough.

Of course, the fay javascript files were a bit larger than the normal
javascript ones. Not too significantly, though, only about 80x. This
actually puts them however at around the size of my image files
(\~100KB)…this is slightly worrisome, but I don’t really stress too much
about one image, so I guess I shouldn’t stress too much about this
either. Not ideal, but what else could I expect?

Future stuff
------------

Hopefully I’m able to make [that javascript
call](http://blog.jle.im/source/code-samples/source/entry_toc.js#L4-21)
on fay one day, without having to rewrite the entire library in Fay
(although it might be a fun exercise).

<!-- ~~~javascript -->
<!-- !!!source/entry_toc.js "#toc"18 -->
<!-- ~~~ -->
If anyone knows how I can do this, I’d really appreciate any help!

I’d also in the future like to make my preprocessor a bit more robust
and take more languages. But…I probably wouldn’t do this until the need
actually arises :)
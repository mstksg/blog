Fun With MonadPlus: The Success/Failure Monads
==============================================

> Originally posted by [Justin Le](http://home.jle0.com:4111/).
> [Read online!](http://home.jle0.com:4111/entry/ident/list-monad.html)

Monads. Haskell’s famous for them, but they are one of the most ill-understood
concepts to the public. They are mostly shrouded in mystery because of their
association with how Haskell models I/O. This reputation is undeserved. Monads
don’t have anything to do with I/O.

This series is a part of a global effort to deshroud the mystery behind monads
and show that they are fun! And exciting! And really just a way of chaining
together functions that allow for new ways of approaching puzzles.

At the end of it all, we are going to be solving the classic logic puzzle, as
old as time itself, using the List monad:

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport across
> a river. Unfortunately, his only boat can carry one thing at a time. He can’t
> leave the wolf alone with the goat, or the wolf will eat the goat. He can’t
> leave the goat alone with the cabbage, or the goat will eat the cabbage. How
> can he properly transport his belongings to the other side one at a time,
> without any disasters?

Let us enter a brave new world!

Monads, Reviewed
----------------

As a Haskell blogger, I’m not allowed to write any straight-up monad tutorials,
but I don’t need too — there are a wealth of great ones. [Adit provides a great
concise
one](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html),
and, if you want, [a more in depth
one](http://www.haskell.org/haskellwiki/All_About_Monads) is on the haskell.org
wiki about using them in real life.

However, here is a short description of “monadic style”, as it applies to what
we are going to do today.

Remember — different monads do not actually have any non-superficial
relationship to one another. When we say monads, we just mean things that chain
together functions “inside” wrappers, containers, or contexts. In our case,
containers.

Maybe, maybe not
----------------

Let’s look at the most obvious container – a `Maybe a`. A `Maybe a` is a
container that can either be `Just a` (representing a succesful result `a`) or a
`Nothing` (representing a failed result).

<aside>
    ###### Aside

Hi! These asides are going to be for you readers that are unfamiliar with
Haskell syntax. Feel free to ignore them if you already feel comfortable.

Anyways, if you’ve ever done any object-oriented programming, you might be able
to think of `Maybe a` as an abstract/virtual superclass with templates/generics
— `Maybe<a>`, kinda. And that superclass has two subclasses: `Just<a>`, which
has one public instance variable of type `a`, and `Nothing`, which contains no
instance variables.
</aside>
Often times you’ll have functions that fail, and you want to chain them. The
easiest way is that any function that is chained onto a failed value will be
skipped; a failure is the final result.

Consider the `halve` function, which returns `` Just (x `div` 2) `` on a
succesful halving, or `Nothing` on an unsuccesful halving:

``` {.haskell}
halve :: Int -> Maybe Int                       -- 1
halve x | even x    = Just (x `div` 2)          -- 2
        | otherwise = Nothing                   -- 3
```

<aside>
    ###### Aside

Hi again! There are some quick syntax features here.

1.  This first line declares that the type of the function `halve` is
    `Int -> Maybe Int`, which means that it takes in an `Int` and returns a
    `Maybe Int` — an integer wrapped in a “Maybe” container.
2.  This says that if x is even, then return a succesful `` Just (x `div` 2) ``.
    `` x `div` 2 `` is basically x divided by two, in case you couldn’t
    guess already.
3.  Otherwise, return `Nothing` — a failure.

</aside>
We can now chain `halve`s on results of other `halves`, and have any failures
automatically propagate to the end and short circuit your entire computation:

``` {.haskell}
λ: halve 8
Just 4
λ: halve 7
Nothing
λ: halve 8 >>= halve
Just 2
λ: halve 7 >>= halve
Nothing                         -- 1
λ: halve 6 >>= halve
Nothing
λ: halve 6 >>= halve >>= halve
Nothing                         -- 2
λ: halve 32 >>= (\_ -> Nothing)
Nothing                         -- 3
λ: halve 32 >>= halve >>= halve >>= halve
Just 2
λ: halve 32 >>= (\_ -> Nothing) >>= halve >>= halve >>= halve
Nothing                         -- 4
```

<aside>
    ###### Aside

In this article, code that begins with `λ:` represents commands to be entered at
the interactive prompt, ghci. Code that doesn’t is actual source code.
</aside>
Some interesting points:

1.  Note that this command doesn’t even bother with the second `halve`. It knows
    that the end result will be `Nothing` no matter what (because `halve 7` is
    `Nothing`), so it just skips right past the second `halve`.
2.  Same thing here; it just skips right past the third `halve`, becuase
    `halve 6 >>= halve` is `Nothing`.
3.  Somewhat interesting. `(\_ -> Nothing)` is a function that returns `Nothing`
    no matter what. So chaining that at the end of `halve 32` (a `Just 12`) sort
    of means instant failure no matter what.
4.  Disaterous! Even though halving 32 four times usually is fine, giving
    `Just 2`, having just one failure along the way means that the entire thing
    is a failure. Think of it as `Nothing >>= halve >>= halve >>= halve`.

Do Notation
-----------

Haskell provides a convenient way of writing chained `>>=`’s called do notation;
here are a few samples matched with their equivalent `>>=` form:

``` {.haskell}
λ: half 8
Just 4
λ: do
 |     half 8
Just 4

λ: halve 8 >>= halve
Just 2
λ: do
 |     x <- halve 8
 |     halve x
Just 2

λ: halve 32 >>= halve >>= halve >>= halve
Just 2
λ: do
 |     x <- halve 32
 |     y <- halve x
 |     z <- halve y
 |     halve z
Just 2

λ: halve 32 >>= (\_ -> Nothing) >>= halve >>= halve
Nothing
λ: do
 |     x <- halve 32
 |     Nothing
 |     y <- halve x
 |     z <- halve y
 |     halve z
Nothing
```

It’s kind of imperative-y — “do `halve 32` and assign the result (16) to `x`…do
`halve x` and assign the result (8) to `y`…” — but remember, it’s still just a
bunch of chained `>>=`’s in the end.

Failure is an option
--------------------

The important thing to note here is that “do” notation basically builds up one
“giant” object. Remember the last two examples — the second to last one, all of
those lines were in an effort to build one giant `Just 2` value. In the last
example, all of those lines were in an effort to build one giant `Nothing`
value. That’s why one `Nothing` “ruined” the whole thing. The entire computation
is one big `Maybe a`. If at any point in your attempt to build that `Maybe a`,
you fail, then you have `Nothing`.

This behavior of “fail anywhere, fail the entire thing” is a special subclass of
Monads. Not all monads are like this. This special subclass of monads, we call
“MonadPlus”. I know, the name isn’t really that fancy and it’s also slightly
embarassing. But a MonadPlus is basically a monad that embodies this ideal of
*“I am building up either a success or a failure…and if at any point I fail, the
whole thing is a failure.”*

What do I mean?

Well, “monads” really is just a way to allow things to return new objects based
on the contents of previous objects. Given any object, there is technically more
than one way to do this, obviously…you can have the “chaining” process be
anything you want, arbitrarily. Sometimes, it’s useful to think of this chaining
as a failure/success process. When we model chaining as a failure/success
process, we say that we model it as a MonadPlus.

When you’re working with a MonadPlus, your failure case is called “empty”. In
fact, you can type in `empty` instead of `Nothing`, and Haskell will know you
mean `Nothing` — it’s like an alias.

Now let’s revisit the last do block and make it more generic, and just rephrase
it in a form that we are going to be encountering more when we solve our problem
with the List monad:

``` {.haskell}
halveThriceOops :: Int -> Maybe Int
halveThriceOops n = do          -- call with n = 32
    x  <- Just n                -- Just 32              -- 1
    y  <- halve x               -- Just 16
    empty                       -- Nothing              -- 2
    z  <- halve x'              -- (skip)               -- 3
    zz <- halve x'              -- (skip)
    return zz                   -- (skip)               -- 4
```

Note that I’ve also included a line-by-line ‘trace’ of the do block with what
the monad “is” at that point. It is what is calculated on that line, and it
would be the value returned if you just exited at that step.

1.  We’re going to “initialize” by setting the whole result to be `Just n`,
    at first. This is slightly redundant becuase this is sort of dummy step — as
    soon as we put `n` in the `Just`, we “take it out” again and put it in `x`.
    (The arrows mean “take the content inside of the `Maybe` and put it in this
    value”) So while this is sort of redudant, the reason for this will be
    clear later. Also, it’s nice to just sort of see a nice “Step 0”
2.  The failure. Remember, `empty` means “fail here automatically”, which, in a
    Maybe object, means `Nothing`.
3.  Now from here on, nothing else even matters…the entire block is a failure!
4.  `return :: a -> Maybe a` — it basically “always succeeds” with the value
    given to it. And if it wasn’t for the fact that the block already failed
    before it could get to this line, it would succeed with `zz`. This, too, is
    slightly redundant, but it’ll also make sense later. Remember, this is just
    a “succeed automatically with this value” — if we had written `return 5`,
    the entire block would have succeeded with a 5 if it didn’t already fail. If
    we had written `return x`, it would have succeeded with the original
    unchanged value!

### Guards

Wouldn’t it be handy to have a function that says “fail right…here. if this
condition is not met”? Sort of like a more judicious `empty`, which says “fail
here no matter what”.

Luckily, Haskell gives us one in the standard library:

``` {.haskell}
guard :: MonadPlus m => Bool -> m ()        -- 1
guard True  = return ()
guard False = empty
```

<aside>
    ###### Aside

1.  This is a type signature, like before. We say that `guard` is a function
    that takes a `Bool` and returns a `m ()` — a monad containing `()`. But we
    say that `m`, the monad, must be a MonadPlus.

    For example, if we applied this to `Maybe`, the concrete signature would be
    `guard :: Bool -> Maybe ()`

</aside>
So `guard` will make sure a condition is met, or else it fails the entire thing.
If the condition is met, then it succeeds automatically and places a `()` in the
value.

We can use this to re-implement `halve`, using do notation:

``` {.haskell}
halve :: Int -> Maybe Int
halve n = do                -- n = 8        n = 7
    x <- return n           -- Just 8       Just 7
    guard $ even x          -- Just ()      Nothing
    return $ x `div` 2      -- Just 4       (skip)
```

<aside>
    ###### Aside

`guard $ even x` is no mystery…it is just shorthand for `guard (even x)`. We
just don’t like writing all those parentheses out.
</aside>
So…halve puts `n` into a `Just` to get it in the context of `Maybe`. Then, if
`x` (which is just `n`) is not even, it fails right there. If not, it
auto-succeeds with `` x `div` 2 ``.

You can trust me when I say this works the exact same way!

As a friendly reminder, this entire block is “compiled”/desugared to:

``` {.haskell}
halve n :: Int -> Maybe Int
halve n = return n >>= (\x -> guard (even x)) >>= return (x `div` 2)
```

<aside>
    ###### Note

Some of this might seem a little convoluted…why didn’t we just do:

    halve :: Int -> Maybe Int
    halve n = do
        guard $ even n
        return $ n `div` 2

The answer is that we could…but just hang on tight for a bit and see why I
didn’t write it this way!
</aside>
Lists
-----

Well, this article is about the List monad and I have done very little to talk
about it at all.

When I say “list monad”, I mean “one way that you can implement chaining
operations on a list”. To be more precise, I should say “haskell’s default
choice of chaining method on lists”. There is no “the list monad”…there is “a
way we can make *list* a monad”.

And one way we can do it? We saw it before — yup! We can model lists as a
MonadPlus — a method of chaining that revolves around successes and failures.

Don’t believe me? Let’s take the exact same `halve` function…but instead of
returning a `Maybe Int`, we returned a list of `Int`s:

``` {.haskell}
halve :: Int -> [Int]
halve n = do
    x <- return n
    guard $ even x
    return $ x `div` 2
```

This is…the exact same function. We didn’t do anything but change the type
signature. But because you believe me when I say that List is a MonadPlus…this
should work, right? `guarad` should work for any MonadPlus.

How is list a meaningful MonadPlus? Simple: a “failure” is an empty list. A
“success” is a non-empty list.

Watch:

``` {.haskell}
λ: halve 8
[4]
λ: halve 7
[]
λ: halve 8 >>= halve
[4]
λ: halve 7 >>= halve
[]
λ: halve 32 >>= halve >>= halve >>= halve
[2]
λ: halve 32 >>= (\_ -> empty) >>= halve >>= halve >>= halve
[]
```

Oh my goodness. `Nothing` is just `[]`…`Just a` is now just `[a]`. It’s al so
clear now. Why does `Maybe` even exist? What an outrage! This whole time! It’s
all a lie!

In fact, if we generalize our type signature for `halve`, we can do some crazy
things…

``` {.haskell}
genericHalve :: MonadPlus m => Int -> m Int
genericHalve n = do
    x <- return n
    guard $ even x
    return x
```

``` {.haskell}
λ: genericHalve 8 :: Maybe Int
Just 4
λ: genericHalve 8 :: [Int]
[4]
λ: genericHalve 7 :: Maybe Int
Nothing
λ: genericHalve 7 :: [Int]
[]
```

<aside>
    ###### Aside

When we say something like `genericHalve 8 :: Maybe Int`, it means “I want
`genericHalve 8`…and I want the type to be `Maybe Int`.” This is necessary here
becuase in our `genericHalve` can be *any* MonadPlus, so we have to tell ghci
which MonadPlus we want.
</aside>
So there you have it. Maybe and lists are one and the same. Lists *do* too
represent the concept of failure and success. So…what’s the difference?

### A List Apart

Lists can model failure the same way that Maybe can. But it should be apparent
that list can model success…very interestingly.

Consider `[3, 5]`. Clearly this is to represent some sort of “success”. But
what?

How about we look at it this way: `[3, 5]` represents two separate *paths* to
success. When we look at a `Just 5`, we see a computation that succeeded with a
5. When we see a `[3, 5]`, we may interpret it as a computation that had two
possible succesful paths: one succeeding with a 3 and another with a 5.

You can also say that it represents a computaiton that *could have chosen* to
succeed in a 3, or a 5. In this way, the list monad is often referred to as the
“choice” monad.

This view of a list as a collection of possible successes or choices of
successes is not the only way to think of a list as a monad…but it is the way
that the Haskell community has adopted as arguably the most useful. (The other
main way is to approach it completely differently, making list not even a
MonadPlus and therefore not representing failure or success at all)

Think of it this way: A value goes through a long and arduous journey with many
choices and possible paths and forks. At the end of it, you have the result of
every path that could have lead to a success. Contrast this to the `Maybe`
monad, where a value goes through this arduous journey, but never has any
choice. There is only one path — succesful, or otherwise. A `Maybe` is
deterministic…a list provides a choice in paths.

Let’s take a simple example: `halveOrDouble`. It provides two succesful paths if
you are even: halving and doubling. It only provides one choice or possible path
to success if you are odd: doubling. In this way it is slightly racist.

``` {.haskell}
halveOrDouble :: Int -> [Int]
halveOrDouble n | even n    = [n `div` 2, n * 2]
                | otherwise = [n * 2]
```

``` {.haskell}
λ: halveOrDouble 6
[3, 6]
λ: halveOrDouble 7
[  14]
```

As you can see in the first case, with the 6, there are two paths to success:
the halve, and the double. In the second case, with the 7, there is only one —
the double.

How about we subject a number to this halving-or-doubling journey twice? What do
we expect?

1.  The path of halve-halve only works if the number is divisible by two twice.
    So this is only a succesful path if the number is divisible by four.
2.  The path of halve-double only works if the number is even. So this is only a
    succesful path in that case.
3.  The path of double-halve will work in all cases! It is a success always.
4.  The path of double-double will also work in all cases…it’ll never fail for
    our sojourning number!

So…halving-or-doubling twice has two possible succesful paths for an odd number,
three succesful paths for a number divisible by two but not four, and four
succesful paths for a number divisible by four.

Let’s try it out:

``` {.haskell}
λ: return 5 >>= halveOrDouble >>= halveOrDouble
[       5, 20]
λ: return 6 >>= halveOrDouble >>= halveOrDouble
[    6, 6, 24]
λ: return 8 >>= halveOrDouble >>= halveOrDouble
[ 2, 8, 8, 32]
```

The first list represents the results of all of the possible succesful paths 5
could have taken to “traverse” the dreaded `halveOrDouble` landscape twice —
double-halve, or double-double. The second, 6 could have emerged succesful with
halve-double, double-halve, or double-double. 8 had the succesful paths it could
have taken.

Let’s look at this in the do notation form to offer some possible insight:

``` {.haskell}
halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do
    x <- return n
    y <- halveOrDouble x
    z <- halveOrDouble y
    return z
```

Do notation describes **a single path of a value**. This is slightly confusing
at first. But look at it — it has the exact same form as a Maybe monad do block.

This thing describes, in general terms, the path of a single value. `x`, `y`,
and `z` are not lists — they represent a single value, in the middle of the
treacherous journey.

Here is an illustration, tracing out “individual paths”:

``` {.haskell}
halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do       -- halveOrDoubleTwice 6
    x <- return n               -- x =              Just 6
    y <- halveOrDouble x        -- y =      Just 3          Just 12
    z <- halveOrDouble y        -- z = Nothing  Just 6  Just 6  Just 24
    return z                    --     Nothing  Just 6  Just 6  Just 24
```

where you take the left path if you want to halve, and the right path if you
want to double.

Remember, just like in the Maybe monad, the `x`, `y`, and `z` represent the
*value* inside the object — `x` represents the 6, `y` represents either the 3 or
the 12, depending on what path you take. This binding of `x`, `y`, or `z`
remains the same throughout the remainder of the path.

Here is the tricky part: the last line, `return z`, returns **what `z` is on
that path**. In the halve-double path, `z` is 6. In the `double-double` path,
`z` is 24.

What if we had typed `return y` instead of `return z`?

``` {.haskell}
halveOrDoubleDance :: Int -> [Int]
halveOrDoubleDance n = do       -- halveOrDoubleDance 6
    x <- return n               -- x <-             Just 6
    y <- halveOrDouble x        -- y <-     Just 3          Just 12
    z <- halveOrDouble y        -- z <- Nothing Just 6  Just 6  Just 24
    return z                    --      Nothing Just 3  Just 12 Just 12
```

``` {.haskell}
λ: halveOrDoubleDance 6
[    3,12,12]
λ: halveOrDoubleDance 7
[      14,14]
λ: halveOrDoubleDance 8
[ 4, 4,16,16]
```

![*halveOrDoubleDance 6*, all journeys
illustrated](/img/entries/list-monad/halvedouble.png "halveOrDoubleDance 6")

Huh. What happened here?

Again, there are four possible paths/journies…only three of them end in success.
In the halve-halve path…it fails. Now let’s see what happens in the
“halve-double” path. In this case, it might be useful to look at the
corresponding Maybe do-block, and using the choices we make explicitly:

``` {.haskell}
halveOrDoubleDance' :: Int -> Maybe Int
halveOrDoubleDance' n = do      -- halveOrDoubleDance' 6
    x <- return n               -- Just 6
    y <- halve x                -- Just 3
    z <- double y               -- Just 6  (double n = Just n)
    return y                    -- Just 3
```

It is clear in this case that `return y` will give you the value of `y` **on
that path**.

In our halve-double path, the value of `y` (which is bound on the second line)
is 3. That’s why when we say `return y`, it is `[3]`.

Remember — you have to treat everything as its own individual path. In the
halve-double path, `y` is 3. So `return y` returns 3.

### Solving real-ish problems

Okay, we are *almost* ready to finally implement our solution to the
Wolf/Goat/Cabbage puzzle. Just one more demonstration.

Let’s try this somewhat practical question:

“What operations on a number will make it a multiple of three?”

``` {.haskell}
isMultThree :: Int -> Bool                              -- 1
isMultThree a = a `mod` 3 == 0

testNumber :: Int -> [String]
testNumber n = do
    x <- return n                                       -- 2
    (f, fName)  <-  [ ((*2)         , "times two")      -- 3
                    , ((*3)         , "times three")
                    , ((+2)         , "plus two")
                    , ((+3)         , "plus three")
                    , ((^2)         , "square")
                    , ((+1).(^2)    , "square plus 1")
                    , ((+1).(^3)    , "cube plus 1")
                    , (id           , "stay the same")
                    ]
    let z = f x                                         -- 4

    guard $ isMultThree z                               -- 5
    return fName                                        -- 6
```

``` {.haskell}
λ: testNumber 4
["times three", "plus two"]
λ: testNumber 5
["times three", "cube plus 1"]
λ: testNumber 6
["times two", "times three", "plus three", "square", "stay the same"]
λ: testNumber 7
["times three", "plus two"]
λ: testNumber 8
["times three", "cube plus 1"]
```

Let’s go over this step-by-step:

1.  First of all, define the utility function `isMultThree a`, which is true
    when `a` is a multiple of three and false when it isn’t.
2.  In the block, `x` is set to be a choice in the journey. This choice is
    always going to be `n`, but if we wanted to test multiple numbers, we could
    do something like `x <- [n, n+1, n+2]`.
3.  Now, the journey digerges. `f` and `fName` is now a value that depends on
    the path we took. If we took the first path, `f = (*2)` (the
    doubling function) and `fName = "times two"`. On the second path, `f = (*3)`
    (the tripling function) and `fName = "times three"`, etc.
4.  We alias `z` to be the function we chose applied to `x`. If we had chosen
    the path `f = (*2)`, `z` would be `(*2) x`, which is `x*2`.
5.  We check if `z` is a multiple of three. If it isn’t, the journey sadly
    ends here. For example, if we called the function with `n = 4`, and we had
    chosen `f = (^2)` (the square function), this journey (involving the choice
    of `(^2)`) would meet its failure here…but the journey with the choice
    `f = (+2)` would not!
6.  At the end of the weary journey, we return the name of the function
    we chose. This step is never reached for failed journeys.

Here is a diagram!

![*testNumber 5*, all journeys
illustrated](/img/entries/list-monad/testnumber.png "testNumber 5")

<!-- ### Maybe? -->
<!-- What do I mean? -->
<!-- Let's look at the most obvious container -- a `Maybe a`.  A `Maybe a` is a -->
<!-- container that can either be `Just a` (representing a succesful result `a`) or -->
<!-- a `Nothing` (representing a failed result). -->
<!-- <aside> -->
<!--     ###### Aside -->
<!-- Hi!  These asides are going to be for you readers that are unfamiliar with -->
<!-- Haskell syntax.  Feel free to ignore them if you already feel comfortable. -->
<!-- Anyways, if you've ever done any object-oriented programming, you might be -->
<!-- able to think of `Maybe a` as an abstract/virtual superclass with -->
<!-- templates/generics --- `Maybe<a>`, kinda.  And that superclass has two -->
<!-- subclasses: `Just<a>`, which has one public instance variable of type `a`, and -->
<!-- `Nothing`, which contains no instance variables. -->
<!-- </aside> -->
<!-- The Monad instance of Maybe is useful because it allows us to chain -->
<!-- failable-computations. -->
<!-- For example, the `halve` function, which returns ``Just (x `div` 2)`` on a -->
<!-- succesful halving, or `Nothing` on an unsuccesful halving: -->
<!-- ~~~haskell -->
<!-- halve :: Int -> Maybe Int                       -- 1 -->
<!-- halve x | even x    = Just (x `div` 2)          -- 2 -->
<!--         | otherwise = Nothing                   -- 3 -->
<!-- ~~~ -->
<!-- <aside> -->
<!--     ###### Aside -->
<!-- Hi again!  There are some quick syntax features here. -->
<!-- 1.  This first line declares that the type of the function `halve` is `Int -> -->
<!--     Maybe Int`, which means that it takes in an `Int` and returns a `Maybe -->
<!--     Int` --- an integer wrapped in a "Maybe" container. -->
<!-- 2.  This says that if x is even, then return a succesful ``Just (x `div` 2)``. -->
<!--     ``x `div` 2`` is basically x divided by two, in case you couldn't guess -->
<!--     already. -->
<!-- 3.  Otherwise, return `Nothing` --- a failure. -->
<!-- </aside> -->
<!-- ~~~haskell -->
<!-- λ: halve 6 -->
<!-- Just 3 -->
<!-- λ: halve 7 -->
<!-- Nothing -->
<!-- ~~~ -->
<!-- <aside> -->
<!--     ###### Aside -->
<!-- In this article, code that begins with `λ: ` represents commands to be entered -->
<!-- at the interactive prompt, ghci.  Code that doesn't is actual source code. -->
<!-- </aside> -->
<!-- How would we write `halveTwice`? -->
<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = -->
<!--     case halve x of                             -- 1 -->
<!--         Just x2 -> halve x2 -->
<!--         Nothing -> Nothing -->
<!-- ~~~ -->
<!-- <aside> -->
<!--     ###### Aside -->
<!-- 1.  Like a case/switch statement in any language, the path it takes depends on -->
<!--     what you give it.  In this case, it calculates `halve x`, and decides with -->
<!--     path depending on what `halve x` is. -->
<!-- </aside> -->
<!-- ~~~haskell -->
<!-- λ: halveTwice 6 -->
<!-- Nothing -->
<!-- λ: halveTwice 8 -->
<!-- Just 2 -->
<!-- ~~~ -->
<!-- Okay, this isn't too clean code.  What about `halveThrice`? -->
<!-- ~~~haskell -->
<!-- halveThrice :: Int -> Maybe Int -->
<!-- halveThrice x = -->
<!--     case halve x of -->
<!--         Just x2 -> -->
<!--             case halve x2 of -->
<!--                 Just x3     -> halve x3 -->
<!--                 Nothing     -> Nothing -->
<!--         Nothing     -> -->
<!--             Nothing -->
<!-- ~~~ -->
<!-- ~~~haskel -->
<!-- λ: halveThrice 4 -->
<!-- Nothing -->
<!-- λ: halveThrice 8 -->
<!-- Just 1 -->
<!-- ~~~ -->
<!-- Now that's just downright ugly. -->
<!-- What are we trying to do here, exactly? -->
<!-- Basically, we want to generate a new `Maybe` based on what a current `Maybe` -->
<!-- contains.  We want to chain these. -->
<!-- Monads to the rescue! -->
<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice = (halve x) >>= halve -->
<!-- halveThrice :: Int -> Maybe Int -->
<!-- halveThrice = (halve x) >>= halve >>= halve -->
<!-- ~~~ -->
<!-- And that's it! -->
<!-- `>>=` takes care of the plumbing (the ugly case statements) for us and -->
<!-- abstracts it away.  Want to know how?  Too bad!  This isn't a monad tutorial! -->
<!-- Read a real one :) -->
<!-- Anyways, the cool thing about monads in Haskell is that Haskell provides -->
<!-- convenient syntactic sugar for using `>>=`: -->
<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = do -->
<!--     x2  <- halve x -->
<!--     halve x2 -->
<!-- ~~~ -->
<!-- which is the same as: -->
<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = halve x >>= (\x2 -> halve x2)    -- 1 -->
<!-- ~~~ -->
<!-- which is -->
<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = halve x >>= halve -->
<!-- ~~~ -->
<!-- <aside> -->
<!--     ###### Aside -->
<!-- 1. Haskell has the construct `(\x -> f x)` which is basically a function that -->
<!--    takes `x` and returns `f x`.  So `(\x2 -> halve x2)` is a function that -->
<!--    takes an `x2` and returns `halve x2`.  This is exactly the same as the -->
<!--    function `halve` --- it takes an `x` and returns `halve x`. -->
<!-- </aside> -->
<!-- We can also do `halveFourTimes`: -->
<!-- ~~~haskell -->
<!-- halveFourTimes :: Int -> Maybe Int -->
<!-- halveFourTimes x = do -->
<!--     x2 <- halve x -->
<!--     x3 <- halve x2 -->
<!--     x4 <- halve x3 -->
<!--     halve x4 -->
<!-- ~~~ -->
<!-- Imagine having to do that manually! -->
<!-- ### The do block -->
<!-- Note one interesting thing about a `>>=` based definition of `halveFourTimes`: -->
<!-- ~~~haskell -->
<!-- halveFourTimes :: Int -> Maybe Int -->
<!-- halveFourTimes = halve x >>= halve >>= halve >>= halve -->
<!-- ~~~ -->
<!-- Note that at any time, if *any* of those `halve`s fail, the entire thing -->
<!-- fails. -->
<!-- This is how `Maybe` works --- if a computation fails, then all computations -->
<!-- deriving from that computation will also fail, necessarily. -->
<!-- Think about something like this: -->
<!-- ~~~haskell -->
<!-- halveFourTimesOops :: Int -> Maybe Int -->
<!-- halveFourTimesOops x = do -->
<!--     x2 <- halve x -->
<!--     x3 <- halve x2 -->
<!--     _  <- Nothing                       -- 1 -->
<!--     x4 <- halve x3 -->
<!--     halve x4 -->
<!-- ~~~ -->
<!-- <aside> -->
<!--     ###### Aside -->
<!-- 1.  An underscore `_` in Haskell is a wildcard; basically, it says "store this -->
<!--     value somewhere, but I don't need it again ever so I won't even bother -->
<!--     giving it a name." -->
<!-- </aside> -->
<!-- Now watch: -->
<!-- ~~~haskell -->
<!-- λ: halveFourTimes 32 -->
<!-- Just 2 -->
<!-- λ: halveFourTimesOops 32 -->
<!-- Nothing -->
<!-- ~~~ -->
<!-- That's what `Maybe` really is --- it chains together failable computations. -->
<!-- But if at any point in time, a computaion fails, then the entire full chained -->
<!-- computation is now a failure, no matter what. -->
<!-- To convince you, let's break down a simple `>>=` style chaining of -->
<!-- `halveTwiceOops`: -->
<!-- ~~~haskell -->
<!-- λ: halve 8 >>= (\_ -> Nothing) >>= halve -->
<!-- Nothing -->
<!-- ~~~ -->
<!-- What is `halve 8 >>= (\_ -> Nothing)`, anyway?  It is "ignore the result of -->
<!-- `halve 8` and return `Nothing` no matter what": -->
<!-- ~~~haskell -->
<!-- λ: halve 8 >>= (\_ -> Nothing) -->
<!-- Nothing -->
<!-- ~~~ -->
<!-- So obviously, we expect `Nothing >>= halve` to return `Nothing`...you can't -->
<!-- halve a failure! -->
<!-- ~~~haskell -->
<!-- λ: Nothing >>= halve -->
<!-- Nothing -->
<!-- ~~~ -->
<!-- So that's why, if at any point along the chain you have a failure, the entire -->
<!-- thing fails. -->
<!-- Okay, now let's get to the actual problem (finally!). -->
<!-- Okay, let's back up.  When we say "functor", we mean things that have the -->
<!-- ability to apply functions "inside" them. -->
<!-- ~~~haskell -->
<!-- -- Normal function application, with $ -->
<!-- λ: (*2) $ 3 -->
<!-- 6 -->
<!-- -- Function application inside a container, with <$> -->
<!-- λ: (*2) <$> [3] -->
<!-- [6] -->
<!-- λ: (*2) <$> Just 3 -->
<!-- -- "Just" is just a container that contains one value, the 3 -->
<!-- Just 6 -->
<!-- λ: (*2) <$> [3,4,5] -->
<!-- [6,8,10] -->
<!-- ~~~ -->
<!-- Note the last one...the List functor...we can say that the list -->
<!-- "simultaneously contains" 3, 4, and 5...just like a Maybe "contains" a 3.  So -->
<!-- when we apply the function `(*2)` (the doubling function) to what is "inside" -->
<!-- a List...we apply it to all things simultaneously inside the list (3, 4, and -->
<!-- 5). -->
<!-- Now, when we say "monad", we mean things that have the ability to create new -->
<!-- objects from the contents of the previous object. -->
<!-- For example, we want to create a `Just something` from the contents of -->
<!-- `Just 5`. So we write a function `f :: Int -> Just Int`, which takes an int -->
<!-- (in this case, the 5) and -->

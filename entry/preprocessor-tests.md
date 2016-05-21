Pre-processor Tests
===================

[Read online!](http://home.jle0.com:4111/entry/preprocessor-tests.html)

*Posted by [Justin Le](http://home.jle0.com:4111/) on January 22, 2014*

Testing the entry pre-processor. It is supposed to expand out code
blocks.

Lorum ipsum blah blah.

``` {.haskell}
-- interactive: https://www.fpcomplete.com/user/jle/wolf-goat-cabbage
onFor :: forall a.
     (a -> Bool)  -- test to see if an input 'triggers'
  -> Int          -- amount of time to stay True for
  -> Auto a Bool  -- An Auto that takes an `a` and returns a `Bool`
onFor p hold = wait
  where
    wait :: Auto a Bool                 -- the "waiting" state
    wait = ACons $ \input ->
      if p input                        -- if triggered,
        then (True, countdown (hold-1)) -- jump to "countdown" state
        else (False, wait)              -- otherwise, stay waiting

    countdown :: Int -> Auto a Bool     -- the "countdown" state
    countdown n = ACons $ \input ->
      if p input                        -- if re-triggered
        then (True, countdown (hold-1)) -- countdown all over again
        else
          if n == 1
            then (False, wait)          -- If counted down, go wait again
            else (True, countdown (n-1))  -- otherwise, count down.

```

``` {.haskell}
data Command k v = Insert k v | Lookup k | Delete k

```

``` {.haskell}
data Command k v = Insert k v | Lookup k | Delete k
```

``` {.haskell}
-- interactive: https://www.fpcomplete.com/user/jle/wolf-goat-cabbage
data Command k v = Insert k v | Lookup k | Delete k

autoMap :: forall k v. Ord k
    => Int              -- the maximum capacity of the map
    -> Auto (Command k v) (Maybe v)
autoMap cap = go Map.empty
  where
    go :: Map.Map k v -> Auto (Command k v) (Maybe v)
    go m = ACons $ \command ->
      case command of
        Insert key val ->
          if Map.size m >= cap && key `Map.notMember` m
            then
              ( Nothing, go m )                 -- Map is full, no go!
            else
              let m' = Map.insert key val m     -- go for it!
              in  ( Just val, go m' )
        Lookup key ->
          ( key `Map.lookup` m, go m )
        Delete key ->
          let result  = key `Map.lookup` m
              m'      = key `Map.delete` m
          in  ( result, go m' )

```

[Autos](/not-found).

<!-- ~~~haskell -->
<!-- !!!machines/Auto.hs -->
<!-- ~~~ -->
<!-- !!!*machines/Auto.hs "autoMap ::" -->

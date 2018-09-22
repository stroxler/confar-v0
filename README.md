# hss

The `hss` project is for me, a haskell beginner, to start learning
some haskell scripting; for the moment I'm mostly focusing on learning
how to use the `Shelly` library.

My ultimate short-term goal is to write a tool similar to the `ghar`
project, to handle my personal config files. I found `ghar` to be super useful,
but it's an abandoned project in python2, and I'd rather use a static
binary for this type of work.

About 6 months back I worked my way through most of Chris Allen and Julie
Moronuki's "Haskell from First Principles", but it's been a while so some
of my difficulties will likely just be silly beginner problems with stack
and other base haskell tools.

# Issues resolved

## Packages

To add a package, e.g. `shelly`, just add it in the list of `dependencies`
in `package.yaml`.

In some cases, it may not work out of the box:
 - At one point I added `text` but stack kept complaining that `Data.Text`
   wasn't defined. I checked `hss.cabal`, and the `text` package wasn't there.
   I'm still not sure what caused this, but I removed `hss.cabal`, and that
   caused `stack build` to regenerate it, which fixed the problem.
 - It hasn't happened in this project, but in some cases (e.g. in my
   Euterpea projects) 

I've added a `make reset-packages` command to do this quickly.

## Type overloading issues

You need overloadedStrings to 

I had a hard time getting going with all the string-like types, especially
`FilePath`.

I still don't fully understand the mechanics at play, but `FilePath` (like
`Data.Text` is an overloaded string type. I couldn't figure out how to
instantiate one directly, I *had* to use the `OverloadedStrings` ghc switch.
To do so:
 - In a file, you use `{-# LANGUAGE OverloadedStrings #-}`
 - In ghci, you use `set -XOverloadedStrings` (it will tab-complete)
 - When starting ghci, you can add `-gch-options -XOverloadedStrings`

I've added a `make ghci` command to make it easy to add pragmas, since I don't
yet know of a way to directly configure stack to do this.


### More musing about OverloadedStrings and FilePath

I still don't understand OverloadedStrings very well, but it seems to cause
string literals to behave similarly to numeric literals. Recall what happens
if you interactively define a literal:
```
ghci> y = 38
ghci> :t y
y :: Num p => p
```
You can force a type, e.g. `z = y :: Integer`, or directly like `z = 55 ::
Int`.

If you have `OverloadedStrings`, you get basically the same thing with a string
literal, e.g.
```
ghci> s = "wow"
gchi> :t s
s :: Data.String.IsString p => p
```
and you can cast either a variable or a literal to a specific type by using,
e.g.  `"wow" :: Text` or `"/kode/" :: FilePath`.

Another issue you can hit pretty easily is that there are two `FilePath` types,
the `System.IO` one and the `Shelly` one. After some poking, I did realize that
there is a function in shelly, `fromText`, that makes a `FilePath` from text.
This may be easier to use than overloading in some cases.

You can convert from the system FilePath (which is really just a String) using
`fromText . pack`, where `pack` converts `String` to `Text`.


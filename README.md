# simplistic `confar`

This project is something of an accident; I originally intended it just as
a repo to play around with haskell for scripting tasks, mainly using the
`shelly` library. I had in mind a few tools that I'd like to have in a
statically compiled language:
 - Tools for bootstrapping and maintaining systems. In particular, I wanted
   a rewritten and more elaborate version of
   [Brandon Phillip's ghar](https://github.com/philips/ghar), which has been
   a dead project for years but I've used to maintain my configuration, and
   I wanted to extend the tool.
 - Tools for bootstrapping environments in dynamic languages, especially
   python. I don't like these tools being written in the target, because
   they wind up relying on a global environment for that dynamic language
   (e.g. your pyenv managment tool is running in some specific python
   interpreter).

I also want to learn haskell scripting more generally because:
 - stack has excellent support for standalone scripts. I was at one time very
   excited by the
   [https://github.com/lihaoyi/Ammonite](ammonite) project in scala for this
   kind of thing, but ammonite is a pretty big departure from standard
   scala build tooling. Stack, on the other hand, is the standard industry
   build tool, so having scripting support built in is a big deal.
 - There are projects to integrate haskell with both neovim and emacs, which
   means that if we get good at scripting development tools in haskell it's
   relatively easy to write editor extensions
 - Haskell is likely to make me better at functional languages that I can
   actually use in my day job, like scala, but it's hard to find full-time
   Haskell jobs. Using it for personal development tooling and such is a great
   way to practice on a weekly basis.

# confar-v0

As mentioned above, I originally intended this repo just to learn some
about shelly, but I wound up with a simplistic reimplementation of `ghar`,
which I'm calling `confar-v0` because I eventually want to write a
`confar` from scratch that's more carefully designed, rather than this
hacked-together learning project.

The executable is called `confar-v0`, and the source is in `app/Main.hs`.
If you run `stack install`, stack will build the project and put `confar-v0` in
a suitable bin directory (it will print the location; you can make sure
that the directory is on your `$PATH`). You can run it with
```
confar-v0 -s <source_repo_path> -t <target_directory>
```
For example, if I set up a new computer, I can install my config files
(which as of September 2018 are mostly in a single repo, although I do
intend to split them once confar works better) by running
```
cd ~
git clone git@github.com:stroxler/config
confar-v0 -s ~/config -t ~
```

# Recording some issues I ran into as a haskell/stack newcomer

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

## Spacemacs errors when loading buffer into repl

I use a few different editors depending on the language I'm working in, but
spacemacs is my goto, and has mostly worked okay for haskell thus far. It
doesn't seem to be able to give me much information from stack alone, probably
because I failed to install `ghc-mod` (which seems to be kind of complicated
to install with `stack` at the moment), but it does work pretty well when
there's a running, managed haskell repl.

Unfortunately at the moment, you have to play with things to make the repl
work very well (it doesn't do much for you unless you load the current buffer).
I was getting
```
Unexpected response from haskell process
```
errors when trying to send the current buffer to the repl in
spacemacs haskell layer, which blocked me pretty badly.

Poking around, I found an
[issue in haskell-mode](https://github.com/haskell/haskell-mode/issues/1553)
that seemed to be related; it looks like the community is working
out changes in ghc that caused some problems for the emacs plugin.

As a temporary fix, it seems that I can get around the problem
by running some combination of `:set -fshow-loaded-modules` and
`:set -fobject-code` in the repl before trying to load a module.

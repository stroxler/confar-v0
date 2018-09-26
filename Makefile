.PHONY: test
	
# unfortunately ghc-mod doesn't currently work with the latest
# stack resolver, and at this point I'm too much of a haskell
# newbie to fix the problem
dev-dependencies:
	stack install hlint hasktags hoogle intero apply-refact

# If a package change from package.yaml isn't taking, you can run this
# to reset the cabal file.
cabal:
	rm *.cabal
	stack build

# Using make ghci makes it easy for us to add language pragmas; it's probably
# possible to configure `stack ghci` to do this automatically but I have not
# found a way.
ghci:
	stack ghci --ghc-options -XOverloadedStrings


test:
	stack test --fast --file-watch

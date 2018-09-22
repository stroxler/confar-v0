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

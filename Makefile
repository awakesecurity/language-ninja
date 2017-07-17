JQUERY = "https://code.jquery.com/jquery-3.2.1.min.js"

.PHONY: nix-shell nix-zsh configure build test doc clean

nix-shell: nix/haskell/language-ninja.nix
	nix-shell shell.nix -Q -j 8 -k

nix-zsh: nix/haskell/language-ninja.nix
	nix-shell shell.nix -Q -j 8 -k --run "zsh"

configure:
	cabal configure --enable-tests

build: configure
	cabal build -j8

test: build
	cabal test -j8

doc:
	-@rm -rf dist/doc
	cabal haddock --hyperlink-source
	rm -vf dist/doc/html/language-ninja/haddock-util.js
	rm -vf dist/doc/html/language-ninja/ocean.css
	cd dist/doc/html/language-ninja;                         \
	    ln -sv ../../../../misc/haddock.js  haddock-util.js; \
	    ln -sv ../../../../misc/haddock.css ocean.css
	curl $(JQUERY) > dist/doc/html/language-ninja/jquery.js

clean:
	cabal clean
	@{ git clean -n; git clean -dn; git clean -Xn; git clean -Xdn; } | sort -u

shell.nix: release.nix

release.nix: nix/haskell

nix/haskell: nix/haskell/language-ninja.nix              \
             nix/haskell/makefile.nix                    \
             nix/haskell/monad-mock.nix                  \
             nix/haskell/prettyprinter.nix               \
             nix/haskell/prettyprinter-ansi-terminal.nix \
             nix/haskell/versions.nix

nix/haskell/language-ninja.nix: language-ninja.cabal
	cd nix/haskell; cabal2nix ../.. > language-ninja.nix

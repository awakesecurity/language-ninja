PACKAGE_NAME = language-ninja
PACKAGE_VERSION = 0.1.0
PACKAGE = $(PACKAGE_NAME)-$(PACKAGE_VERSION)

JQUERY_VERSION = 3.2.1
JQUERY_URL = https://code.jquery.com/jquery-$(JQUERY_VERSION).min.js
JQUERY = misc/jquery-$(JQUERY_VERSION).js

HACKAGE_USERNAME = taktoa

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

doc: $(JQUERY)
	-@rm -rf dist/doc
	cabal haddock --hyperlink-source
	rm -vf dist/doc/html/language-ninja/haddock-util.js
	rm -vf dist/doc/html/language-ninja/ocean.css
	cd dist/doc/html/language-ninja;                         \
	    ln -sv ../../../../misc/haddock.js  haddock-util.js; \
	    ln -sv ../../../../misc/haddock.css ocean.css
	cp -v $(JQUERY) dist/doc/html/language-ninja/jquery.js

doc-tar: $(JQUERY)
	-@rm -rf dist/doc dist/$(PACKAGE)-docs.tar.gz
	-@rm -rf ./$(PACKAGE)-docs/
	cabal haddock --for-hackage
	tar -xzf dist/$(PACKAGE)-docs.tar.gz
	cp -v $(JQUERY)          $(PACKAGE)-docs/jquery.js
	cp -v ./misc/haddock.js  $(PACKAGE)-docs/haddock-util.js
	cp -v ./misc/haddock.css $(PACKAGE)-docs/ocean.css
	-@rm -rf dist/$(PACKAGE)-docs.tar.gz
	tar -czv -f $(PACKAGE)-docs.tar.gz -H ustar $(PACKAGE)-docs
	-@rm -rf $(PACKAGE)-docs
	cp -v $(PACKAGE)-docs.tar.gz dist/$(PACKAGE)-docs.tar.gz
	-@rm -f $(PACKAGE)-docs.tar.gz

doc-candidate-upload: doc-tar
	cp -v dist/$(PACKAGE)-docs.tar.gz .
	curl -X PUT                                       \
	     -H "Content-Type: application/x-tar"         \
	     -H "Content-Encoding: gzip"                  \
	     -u "$(HACKAGE_USERNAME)"                     \
	     --data-binary "@$(PACKAGE)-docs.tar.gz"      \
	     "https://hackage.haskell.org/package/$(PACKAGE)/candidate/docs"
	rm -v "$(PACKAGE)-docs.tar.gz"

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

$(JQUERY):
	curl $(JQUERY_URL) > $(JQUERY)

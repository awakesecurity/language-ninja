# -*- coding: utf-8; mode: makefile; indent-tabs-mode: nil; -*-

.RECIPEPREFIX = >

PACKAGE_NAME = language-ninja
PACKAGE_VERSION = 0.2.0
PACKAGE = $(PACKAGE_NAME)-$(PACKAGE_VERSION)

JQUERY_VERSION = 3.2.1
JQUERY_URL = https://code.jquery.com/jquery-$(JQUERY_VERSION).min.js
JQUERY = misc/jquery-$(JQUERY_VERSION).js

HACKAGE_USERNAME = taktoa

NIXPKGS_REVISION = 694529e45d92fe3ca28c7aa78f57ee4b11d4bb73

all: help

help:
> -@printf "Available targets:\n"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "nix-shell" "enter a nix-shell"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "configure" "cabal configure"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "build"     "cabal build"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "test"      "cabal test"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "haddock"   "cabal haddock"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "candidate" "upload a package candidate"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "upload"    "upload this package"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "clean"     "clean the working directory"
> -@printf "    \e[1m%s\e[0m\t- %s\n" "bump-nix"  "update nixpkgs revision"

nix-shell: nix/haskell/language-ninja.nix
> nix-shell shell.nix -Q -j 8 -k --run $$SHELL

configure:
> cabal configure --enable-tests

build: configure
> cabal build -j8

test: build
> cabal test -j8

haddock: $(JQUERY)
> -@rm -rf dist/doc
> cabal haddock --haddock-option="--hyperlinked-source"
> rm -vf dist/doc/html/language-ninja/haddock-util.js
> rm -vf dist/doc/html/language-ninja/ocean.css
> cd dist/doc/html/language-ninja;                         \
>     ln -sv ../../../../misc/haddock.js  haddock-util.js; \
>     ln -sv ../../../../misc/haddock.css ocean.css
> cp -v $(JQUERY) dist/doc/html/language-ninja/jquery.js

candidate:
> make pkg-candidate-upload
> make doc-candidate-upload

upload:
> make pkg-upload
> make doc-upload

doc-tar: $(JQUERY)
> -@rm -rf dist/doc dist/$(PACKAGE)-docs.tar.gz
> -@rm -rf ./$(PACKAGE)-docs/
> cabal haddock --for-hackage --haddock-option="--hyperlinked-source"
> tar -xzf dist/$(PACKAGE)-docs.tar.gz
> cp -v $(JQUERY)          $(PACKAGE)-docs/jquery.js
> cp -v ./misc/haddock.js  $(PACKAGE)-docs/haddock-util.js
> cp -v ./misc/haddock.css $(PACKAGE)-docs/ocean.css
> -@rm -rf dist/$(PACKAGE)-docs.tar.gz
> tar -czv -f $(PACKAGE)-docs.tar.gz -H ustar $(PACKAGE)-docs
> -@rm -rf $(PACKAGE)-docs
> cp -v $(PACKAGE)-docs.tar.gz dist/$(PACKAGE)-docs.tar.gz
> -@rm -f $(PACKAGE)-docs.tar.gz

doc-upload: pre-push doc-tar
> cp -v dist/$(PACKAGE)-docs.tar.gz .
> -@printf "\e[1mWARNING: THIS IS NOT A CANDIDATE UPLOAD\e[0m\n" > /dev/stderr
> -@printf "Press <ctrl>-c any time in the next 5 seconds to stop this.\n"
> -@sleep 5
> curl -X PUT                                  \
>      -H "Content-Type: application/x-tar"    \
>      -H "Content-Encoding: gzip"             \
>      -u "$(HACKAGE_USERNAME)"                \
>      --data-binary "@$(PACKAGE)-docs.tar.gz" \
>      "https://hackage.haskell.org/package/$(PACKAGE)/docs"
> rm -v "$(PACKAGE)-docs.tar.gz"

doc-candidate-upload: doc-tar
> cp -v dist/$(PACKAGE)-docs.tar.gz .
> curl -X PUT                                  \
>      -H "Content-Type: application/x-tar"    \
>      -H "Content-Encoding: gzip"             \
>      -u "$(HACKAGE_USERNAME)"                \
>      --data-binary "@$(PACKAGE)-docs.tar.gz" \
>      "https://hackage.haskell.org/package/$(PACKAGE)/candidate/docs"
> rm -v "$(PACKAGE)-docs.tar.gz"

pkg-tar: configure
> cabal sdist

pkg-upload: pre-push pkg-tar
> cp -v dist/$(PACKAGE).tar.gz .
> -@printf "\e[1mWARNING: THIS IS NOT A CANDIDATE UPLOAD\e[0m\n" > /dev/stderr
> -@printf "Press <ctrl>-c any time in the next 5 seconds to stop this.\n"
> -@sleep 5
> curl -X POST                         \
>      -F "package=@$(PACKAGE).tar.gz" \
>      -u "$(HACKAGE_USERNAME)"        \
>      "https://hackage.haskell.org/packages"
> -@rm -v "$(PACKAGE).tar.gz"
> -@printf "\e[1m\e[32mRemember to create a git tag for this revision\e[0m\n"

pkg-candidate-upload: pkg-tar
> cp -v dist/$(PACKAGE).tar.gz .
> curl -X POST                         \
>      -F "package=@$(PACKAGE).tar.gz" \
>      -u "$(HACKAGE_USERNAME)"        \
>      "https://hackage.haskell.org/packages/candidates"
> rm -v "$(PACKAGE).tar.gz"

pre-push:
> cabal clean
> test -n "$(git clean -n)"   && exit -1
> test -n "$(git clean -dn)"  && exit -1
> test -n "$(git clean -Xn)"  && exit -1
> test -n "$(git clean -Xdn)" && exit -1
> make test
> cabal check

clean:
> cabal clean
> @{ git clean -n; git clean -dn; git clean -Xn; git clean -Xdn; } | sort -u

bump-nix:
> nix-prefetch-git "https://github.com/NixOS/nixpkgs.git" \
>     "$(NIXPKGS_REVISION)" > nix/nixpkgs.json

shell.nix: release.nix

release.nix: nix/haskell

nix/haskell: nix/haskell/language-ninja.nix              \
             nix/haskell/makefile.nix                    \
             nix/haskell/monad-mock.nix                  \
             nix/haskell/prettyprinter.nix               \
             nix/haskell/prettyprinter-ansi-terminal.nix \
             nix/haskell/versions.nix

nix/haskell/language-ninja.nix: language-ninja.cabal
> cd nix/haskell; cabal2nix ../.. > language-ninja.nix

$(JQUERY):
> curl $(JQUERY_URL) > $(JQUERY)

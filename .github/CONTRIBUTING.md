# Contributing

## Commits

Rules for contribution:

- 80-character column maximum for any file that is intended for humans to read
  or edit. Automatically generated files and test data are exempt from this.
- Single-constructor data type declarations should look like this:
  ```
    -- | FIXME: doc
    data Foo
      = MkFoo
        { _fooBar :: !Bar
        , _fooBaz :: !(Maybe Baz)
        }
      deriving ()

    -- | FIXME: doc
    {-# INLINE makeFoo #-}
    makeFoo :: Bar -> Foo
    makeFoo bar = MkFoo
                  { _fooBar = bar
                  , _fooBaz = Nothing
                  }

    -- | FIXME: doc
    {-# INLINE fooBar #-}
    fooBar :: Lens' Foo Bar
    fooBar = lens _fooBar
             $ \(MkFoo {..}) x -> MkFoo { _fooBar = x, .. }

    -- | FIXME: doc
    {-# INLINE fooBaz #-}
    fooBaz :: Lens' Foo (Maybe Baz)
    fooBaz = lens _fooBaz
             $ \(MkFoo {..}) x -> MkFoo { _fooBaz = x, .. }
  ```
  where the exported symbols are `Foo, makeFoo, fooBar, fooBaz`.
- Multi-constructor data type declarations should look like this:
  ```
    -- | FIXME: doc
    data Foo
      = Bar
      | Baz !Int
      | Quux !Bool !Bool
      deriving ()

    -- | FIXME: doc
    {-# INLINE _Bar #-}
    _Bar :: Prism' Foo ()
    _Bar = prism' (const Bar)
           $ \case Bar -> Just ()
                   _   -> Nothing
  ```
- If reasonable, try to minimize the number of data types defined per module,
  and in general it should be possible to import each type "on its own" without
  using an import list.
- No orphan instances are allowed in the library. They are okay in executables
  and tests, though they should be avoided if possible.
- Unless there is a performance justification for laziness, record fields
  should be declared strict, since GHC does not have warnings for uninitialized
  lazy fields, and that can be a source of bugs.
- Any optics defined should have tests defined using `tasty-lens`, unless it is
  infeasible to define `Serial`/`CoSerial` instances for the relevant types.
- All types should have `Eq`, `Ord`, `Generic`, `Hashable`, and `NFData`
  instances, unless there is some reason that that is not desirable.
  If it is reasonable, polymorphic types should have a `Functor` instance.
- The first line of a commit message should be 73 columns max.
- Before anything is merged into `master`, the following must be true:
  - The code must compile with `-Weverything -Werror`.
  - The tests must be run, and any regressions must be documented.
  - For each file `foo`, `stylish-haskell -i foo` must have no effect.
  - Haddock coverage must be 100% in all public-facing modules.
    If you don't want to add documentation to something, you must at least add
    a `-- | FIXME: doc` comment. This allows me to address documentation all at
    once in the future.
- All of the requirements above do not apply to legacy code written before the
  first Hackage release. Obviously there are efforts to have all of that legacy
  code cleaned up, however.
- Reference any relevant issues in your commit messages.

You can use GitHub pull requests or just email me patches directly using
`git format-patch`; use whatever you are more comfortable with.

<!--
FIXME: add information

One nice aspect of submitting a pull request is that
[travis-ci.org](http://travis-ci.org) bots will automatically merge, build
and run tests against your commits, and continue as you update the request,
so you can be sure you didn't typo stuff or something before a final merge.
-->

For multi-commit requests, your code may get squashed into the smallest possible
logical changes and committed with author attribution in some cases. In general,
try to keep the history clean of things like "fix typo", and this won't normally
be necessary.

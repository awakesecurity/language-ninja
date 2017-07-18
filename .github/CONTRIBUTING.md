# Contributing

## Commits

Rules for contribution:

- 80-character column maximum for any file that is intended for humans to read
  or edit. Automatically generated files and test data are exempt from this.
- In general, try to approximate the style of the surrounding code.
- Use a two space indent in Haskell and Nix files, four spaces otherwise.
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
    makeFoo :: Bar -- ^ FIXME: doc
            -> Foo
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
- Do not export constructors or record fields.
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

    -- | FIXME: doc
    {-# INLINE _Baz #-}
    _Baz :: Prism' Foo Int
    _Baz = prism' Baz
           $ \case (Baz i) -> Just i
                   _       -> Nothing

    -- | FIXME: doc
    {-# INLINE _Quux #-}
    _Quux :: Prism' Foo (Bool, Bool)
    _Quux = prism' Quux
            $ \case (Quux a b) -> Just (a, b)
                    _          -> Nothing
  ```
- If reasonable, try to minimize the number of data types defined per module,
  and in general it should be possible to import each type "on its own" without
  using an import list.
- No orphan instances are allowed in the library. They are okay in executables
  and tests, though they should be avoided if it is reasonable to do so.
- Unless there is a performance justification for laziness, record fields
  should be declared strict, since GHC does not have warnings for uninitialized
  lazy fields, and that can be a source of bugs.
- Any optics defined should have tests defined using `tasty-lens`, unless it is
  infeasible to define `Serial`/`CoSerial` instances for the relevant types.
- All types should have `Eq`, `Ord`, `Generic`, `Hashable`, and `NFData`
  instances, unless there is some reason that that is not desirable.
  If it is reasonable, polymorphic types should have `Functor`, `Foldable`,
  and `Traversable` instances.
- The first line of a commit message should be 73 columns max.
- Before anything is merged into `master`, the following must be true:
  - The code must compile with no warnings.
  - The tests must successfully run.
  - For each file `foo`, `stylish-haskell -i foo` must have no effect.
  - Haddock coverage must be 100% in all public-facing modules.
    If you don't want to add documentation to something, you must at least add
    a `-- | FIXME: doc` comment. This allows me to address documentation all at
    once in the future.
- Reference any relevant issues in your commit messages.
- I am working on getting CI set up (in particular, I need a Hydra server).

For multi-commit requests, your code may get squashed into the smallest possible
logical changes and committed with author attribution in some cases. In general,
try to keep the history clean of things like "fix typo", and this won't normally
be necessary.

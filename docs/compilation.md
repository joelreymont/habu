# Compilation

How Habu turns source into native code, from the ground up. Each layer states
what it owns, what it takes from the layer below and what it gives the layer
above. The decisions are the rules the architecture follows; where the code
does not follow one yet, its **Now** line says what differs.

## Decisions

1. **One dictionary.** The engine's dictionary record is the only table of
   words. A word's type hangs off its record: the checker's per-word data is
   keyed by record index, held with the record, cleared wherever the engine
   writes or discards a record, and captured with the image. A word's package
   and visibility come from its record's wordlist.
   **Now:** the checker keeps a second table keyed by package, visibility and
   name (`src/core/checker.f` `SYM-FIND`, `HIDX`), with its own rollback and
   its own copy of the used package names.
2. **A word's type comes from its definition.** The effect declared where a
   word is defined is its type; nothing restates it elsewhere.
   **Now:** `src/core/checker.f` restates the types of words compiled before
   the checker in `PRIM:`/`PPRIM:` rows.
3. **The kernel.** The kernel is the words compiled before the type checker
   runs. It is untyped and implicitly trusted, and integration tests cover it;
   there is no census and no certification. A kernel word that checked code
   calls carries a trusted type declared at its definition. Every other kernel
   word has no type, is private to the system package and is stripped from the
   delivered `hb`.
4. **Privacy is the only restriction.** A word not everyone may call is private
   to its package. System words are private words of the system package, and
   the build strips private words from the delivered `hb`. There are no owner
   rows, caller checks or marks on records.
   **Now:** engine-internal primitives sit in the global dictionary and are
   refused by marks (`DNAME-INT`, the trusted-only flag, `DNAME-OWNED`);
   `PPRIM:`/`EPPRIM:` owner rows type words per package.
5. **One package seal guard.** Reopening a sealed package is refused by one
   implementation.
   **Now:** `src/habu/packages.f` `PKG-SEAL-GUARD` and `src/habu/habu2.f`
   `C-PACKAGE-SEAL-GUARD` both exist.
6. **Two ways to build an engine.** The Gforth bootstrap makes one from zero.
   The snapshot build (`tools/native-build.f`) loads the sources into a running
   engine and captures its memory; it alone makes the product. The test suite
   builds `hb` once and runs every test against it. The two-generation build
   and the Gforth bootstrap are occasional integration tests.
   **Now:** `tools/build-fixpoint.f` also compiles glued source text to a byte
   fixpoint, and test files build engines of their own.

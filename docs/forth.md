# Forth Standards (habu)

How we write Forth in this repo. **BLOCKING** — code that violates these is wrong,
not a matter of taste. Target is Gforth 0.7.9; gforth-specific gotchas are at the
bottom.

## Naming

- **Our words UPPER-CASE; built-in gforth words as-is.** Words we define
  (`RESOLVE`, `MK-CON`, `APPLY-EFFECT`) are UPPER-CASE; core gforth words stay
  lower-case (`and`, `cells`, `allot`, `: ;`, `?do`). Never upper-case a built-in.
- **Hyphens, never underscores.** `T-CON`, `TV-RESET`, `MAX-TV` — not `T_CON`.
  Underscores are not idiomatic Forth.
- **Conventional suffixes/prefixes**: predicates end `?` (`TYVAR?`); conversions
  `>X` (`TERM>TAG`, `S>NUMBER?`); fetch/store `X@` / `X!` (`TV@` / `TV!`);
  allocate/reset `X-ALLOC` / `X-RESET`.
- Short names per the global naming rules: abbreviate common terms (`buf`, `ctx`,
  `idx`, `nv`, `ki`, `ko`); single letters are fine in tight scope only when
  they do not collide with built-ins. Do not use loop words such as `i` or `j`
  as locals.
- **Check for collisions with gforth built-ins** before naming — gforth is
  case-insensitive, so `CON?`/`VAR?` clash with existing words. Prefix to
  disambiguate (`TYCON?`, `TYVAR?`). When in doubt, `' NAME` in a REPL: if it
  resolves, the name is taken.

## Words & factoring

- **Default new public/library Forth to checked typed definitions.** If the
  checker can express the layer, write an explicit typed effect and let `hb`
  verify it, e.g. `: SQUARE ( i64 -- i64 ) dup * ;`.
- **Unchecked code is a named boundary, not a habit.** Use `0 set-check`,
  raw emitter words, or `TRUST` only for layers the checker cannot express:
  metaprogramming, source-string generators, primitive emitters, snapshot/build
  drivers, and similarly low-level support. Keep the boundary obvious in the
  file and add focused tests for the contract it asserts.
- **Factor reusable helpers back into checked Forth.** If an unchecked harness or
  tool grows a helper that can be typed, move that helper to checked code instead
  of letting unchecked scaffolding become the library surface.
- **Small, single-purpose words**, aim ≤ 5 lines. A word should read top-to-bottom
  without you tracking more than a few stack items.
- **Factor when the stack gets unreadable.** If you reach for `ROT -ROT PICK
  ROLL`, stop and either factor a helper or use locals. Deep juggling is exactly
  what this project forbids in *user* code — hold our own code to it.
- **Locals `{: a b :}`** are encouraged where they remove juggling (gforth 0.7.9
  supports them). Prefer named locals over a 4-deep stack dance.

## Files

- **One concern per file.** Do not bundle unrelated responsibilities: parser,
  renderer, DB, data table, and driver code belong in separate files. Split at
  responsibility boundaries so review stays focused and files can be built in
  parallel.
- **Reusable helpers belong in libraries, not pasted drivers.** Tool/script
  drivers can concatenate library files for `hb script.f args...`, but shared
  behavior should still live in one owned source file.

## Stack comments

- **Every definition** carries `( before -- after )` with items named:
  `( t -- tag )`, `( c-addr u -- nt|0 )`, `( nv -- base )`.
- Add inline `( … )` at non-obvious points inside a longer word so the reader can
  re-anchor mid-definition.
- Use standard notation: `x` cell, `n`/`u` signed/unsigned, `d` double, `c-addr u`
  string, `xt` execution token, `nt` name token, `f` flag, `?` for maybe-present.

## Errors

- **Fallible words `throw` a named code** (defined in `src/config.fs`,
  e.g. `E-MISMATCH`); they never fail silently or return an out-of-band flag in
  place of an error.
- **`catch` only at boundaries** (the `:` override wrapper). No `… catch drop`,
  `catch 2drop`, or other masking — that is forbidden by the global error rules.
- `unreachable`-style `abort"` only for proven-impossible states, with a message.

## Constants

- **Named constants, no magic numbers.** Limits and codes live in `src/config.fs`.
  A literal in code is only acceptable for true primitives of the encoding
  (e.g. the `3`/`7` of the 3-bit tag, and even those get a comment).
- **Prefer `$hex` literals** for bit masks, instruction encodings, ASCII codes, and
  memory offsets (`$FF and`, `$D10043FF`, `$200`); plain counts/indices stay decimal.
  Both gforth and the standalone parse `$hex` (case-insensitive, optional leading `-`).

## Testing (BLOCKING)

- **Every word is exercised by `T{ … -> … }T`** as it's written — happy path plus
  each error/edge. A word without a test is unfinished.
- Tests live in `test/t-<file>.fs`; the harness is `test/tester.fs` (vendored
  Hayes `ttester.fs`).
- Assert the **specific** outcome: for errors, `' WORD catch` and check the exact
  THROW code; for diagnostics, capture text and match a substring.
- Run a single file's tests during dev:
  `gforth src/config.fs … src/<file>.fs test/tester.fs test/t-<file>.fs -e bye`.

## Comments & hygiene

- `\` line comments, terse. No restating what the code obviously does.
- Remove scratch/debug prints before commit.

## Gforth 0.7.9 gotchas that shape how we write code

(Build/environment findings are in `../LESSONS.md`; these are the ones that affect
*coding*.)

- **Case-insensitive** dictionary → name-collision risk (see Naming).
- **`[']` is compile-only.** Inside an interpreted `T{ … }T`, use `'` (tick) to
  get an xt, e.g. `' WORD catch`.
- **`if`/`else`/`then` and `;` are compile-only** — all conditional/looping logic
  must live inside a `:` definition, never at the top level.
- **`parse-name` returns a transient `( c-addr u )`** that the next
  `s"`/`."`/`refill` invalidates — `move` the bytes into your own buffer
  immediately; never hold the pointer across another parsing word.
- **`s>number? ( c-addr u -- d flag )` returns a double** — narrow with `d>s`.
- **"is it a defined word?"** → `find-name ( c-addr u -- nt|0 )`, not `find`.
- **`catch` preserves the pre-call args** under the throw code: `nv ' WORD catch`
  on a throw leaves `( nv code )` — `nip`/adjust in tests accordingly.
- Run tests from a **`.fs` file**, not `echo … | gforth` (which swallows stdout);
  the test exit code is owned by `test/all.fs` (a failed `T{}T` does NOT make
  `gforth -e bye` exit nonzero).

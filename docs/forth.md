# Forth Standards (habu)

How we write Forth in this repo. **BLOCKING** — code that violates these is wrong,
not a matter of taste. Target is the native `bin/hb` engine.

## Naming

- **Our words UPPER-CASE; built-in Forth words as-is.** Words we define
  (`RESOLVE`, `MK-CON`, `APPLY-EFFECT`) are UPPER-CASE; core Forth words stay
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
- **Check for collisions with built-ins** before naming — Forth dictionaries are
  case-insensitive here, so `CON?`/`VAR?` clash with existing words. Prefix to
  disambiguate (`TYCON?`, `TYVAR?`). When in doubt, `' NAME` in a REPL: if it
  resolves, the name is taken.
- **Do not shadow native primitive names.** Later dictionary entries can replace
  primitive signatures and codegen hooks; `shadow-lint` gates this class of bug.

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
- **Build checked task vocabulary before fighting syntax.** If a test, tool, or
  benchmark needs structured rows, JSON/TSV fragments, generated source,
  diagnostics, packets, or repeated assertions, factor domain words or a focused
  checked DSL first. Giant `s"` literals, fragile escaping, and private byte
  emitters are bugs unless they are the tested boundary of that DSL.
- **Small, single-purpose words**, aim ≤ 5 lines. A word should read top-to-bottom
  without you tracking more than a few stack items.
- **Factor when the stack gets unreadable.** If you reach for `ROT -ROT PICK
  ROLL`, stop and either factor a helper or use locals. Deep juggling is exactly
  what this project forbids in *user* code — hold our own code to it.
- **Locals `{: a b :}`** are encouraged where they remove juggling. They bind
  inputs only; do not put `-- outputs` inside the locals form. Keep the effect in
  the stack comment.
- **Do not build deep locals stacks.** Habu locals are reliable for shallow
  factoring; nested helper calls from loop/callback bodies should use stack-based
  leaf helpers or explicitly separate scratch variables so inner helpers cannot
  clobber caller indexes.

## Files

- **One concern per file.** Do not bundle unrelated responsibilities: parser,
  renderer, DB, data table, and driver code belong in separate files. Split at
  responsibility boundaries so review stays focused and files can be built in
  parallel.
- **Reusable helpers belong in libraries, not pasted drivers.** Run multi-file
  tools as `hb --load lib/a.f lib/b.f tool.f -- args...`; the native loader
  appends the source files before `--`, `SCRIPT-ARGV$` starts after it, and fd 0
  remains available as tool data even when stdin is non-tty. Shared behavior
  still lives in one owned source file.
- **Keep script argv explicit.** `hb tool.f arg...` preserves single-script
  compatibility and treats `arg...` as script arguments. Use `--load` only when
  the command line contains more than one source file.

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
- **Interactive/REPL support recovers; builders may exit.** Recoverable
  interactive failures should `throw` into REPL recovery (`?`, rollback, reread).
  Use process exits such as `die` for build-time makers and CLI boundaries where
  terminating the process is the contract.

## Constants

- **Named constants, no magic numbers.** Limits and codes live in `src/config.fs`.
  A literal in code is only acceptable for true primitives of the encoding
  (e.g. the `3`/`7` of the 3-bit tag, and even those get a comment).
- **Prefer `$hex` literals** for bit masks, instruction encodings, ASCII codes, and
  memory offsets (`$FF and`, `$D10043FF`, `$200`); plain counts/indices stay decimal.
  The standalone parses `$hex` (case-insensitive, optional leading `-`).

## Testing (BLOCKING)

- **Every word is exercised by `T{ … -> … }T`** as it's written — happy path plus
  each error/edge. A word without a test is unfinished.
- Tests live in the native gate: `test/engine-suite.f`, focused `tools/*-test.f`
  fixtures, and source-specific checks wired through `test/run.f`.
- Assert the **specific** outcome: for errors, `' WORD catch` and check the exact
  THROW code; for diagnostics, capture text and match a substring.
- Run focused fixtures during dev with their owning `tools/*-test.f`, then run
  the full native gate: `bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f`.

## Comments & hygiene

- `\` line comments, terse. No restating what the code obviously does.
- Remove scratch/debug prints before commit.
- If a definition fails to compile in raw engine mode, Habu reports the undefined
  word on stderr and then may spill the rest of that definition through the
  interpreter. The native `tools/check.f` runner with `--json-errors
  --all-errors` wraps matched undefined tokens in schema-1 JSON diagnostics.

## Habu Native Tooling Gotchas

- **Large native tool bundles are supported.** Do not split tools merely to dodge
  DATA pressure. `create ... allot` is for dictionary-sized static storage; large
  runtime-sized buffers use `lib/memory.f` (`MEM-ALLOC-BYTES` or
  `MEM-ALLOC-64K-BUFFERS`) so composition scales with OS-backed mappings rather
  than `DATA-SIZE`. Tools may keep as many 64K buffers and live spans as their
  workload needs; the only accepted limits are cell-size overflow checks and an
  explicit OS allocation failure. If ordinary composition still hits capacity,
  fix the shared memory model and add a regression for the composed load.
- **Missing convenience words are not bugs in the standard.** Habu currently lacks
  words such as `pick`, `within`, and `0<>`; use variables, explicit increments, or
  explicit comparisons.
- **Trust is audited, not permanent.** `TRUST` records asserted effects so callers
  can be checked, but audit rows must stay current and stale dates must fail lint.

## Native Forth Gotchas That Shape How We Write Code

(Build/environment findings are in `../LESSONS.md`; these are the ones that affect
*coding*.)

- **Case-insensitive** dictionary -> name-collision risk (see Naming).
- **`[']` is compile-only.** In interpreted tests, use `'` (tick) to get an xt,
  e.g. `' WORD catch`.
- **Control words and ticks are compile-only** — `if`/`else`/`then`,
  `begin`/`while`/`repeat`, `[']`, `i`, `?do`, and `;` must live inside a
  `:` definition, never at the top level.
- **`parse-name` returns a transient `( c-addr u )`** that the next
  `s"`/`."`/`refill` invalidates — `move` the bytes into your own buffer
  immediately; never hold the pointer across another parsing word.
- **`s>number? ( c-addr u -- d flag )` returns a double** — narrow with `d>s`.
- **`s" "` is empty, not a one-space string.** The parser consumes the delimiter
  after `s"`, so generated-source builders that need a literal space should emit
  byte `32` or use an existing `*-SP` byte helper.
- **"is it a defined word?"** → `find-name ( c-addr u -- nt|0 )`, not `find`.
- **`catch` preserves the pre-call args** under the throw code: `nv ' WORD catch`
  on a throw leaves `( nv code )` — `nip`/adjust in tests accordingly.
- Run tests through the owning gate script so assertion failures control the
  process exit code.
- **Fallible value-returning scanners should validate first.** Put range/schema
  checks that can `throw` in a `--` helper, then make the value-returning word's
  remaining path structurally return its declared outputs. A final throw-only
  fallback in a word declared as `-- value...` can confuse path-effect merging.

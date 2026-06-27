# Forth Standards (habu)

How we write Forth in this repo. **BLOCKING** — code that violates these is wrong,
not a matter of taste. Target is the native `bin/hb` engine.

Durable Forth language guidance belongs here, not in `LESSONS.md`. Lessons may
record the incident that taught a rule, but the reusable rule itself lives in this
file.

## Naming

- **Our words UPPER-CASE; built-in Forth words as-is.** Words we define
  (`RESOLVE`, `MK-CON`, `APPLY-EFFECT`) are UPPER-CASE; core Forth words stay
  lower-case (`and`, `cells`, `allot`, `: ;`, `?do`). Never upper-case a built-in.
- **Hyphens, never underscores — in word names *and* file names.** `T-CON`,
  `TV-RESET`, `MAX-TV` — not `T_CON`. Source files too: `camera-tracker.f`,
  `latency-xcorr.f`, `timestamp-metrics.f` — not `camera_tracker.f`. Underscores
  are not idiomatic Forth, even when the file ports an underscore-named source from
  another language; name the Habu file in Habu style.
- **Conventional suffixes/prefixes**: predicates end `?` (`TYVAR?`); conversions
  `>X` (`TERM>TAG`, `S>NUMBER?`); fetch/store `X@` / `X!` (`TV@` / `TV!`);
  allocate/reset `X-ALLOC` / `X-RESET`.
- Short names per the global naming rules: abbreviate common terms (`buf`, `ctx`,
  `idx`, `nv`, `ki`, `ko`); single letters are fine in tight scope only when
  they do not collide with built-ins. Do not use loop words such as `i` or `j`
  as locals — a `{: i :}` local shadows the loop-index word and fails with a
  baffling signature: the engine emits the bare token (`i`) to stdout and exits
  `75` with no checker diagnostic. If a definition load prints a lone built-in
  word name and dies, suspect a local that collides with `i`/`j`/`k`. Use `idx`,
  `ix`, `ji`, etc.
- **Check for collisions with built-ins** before naming — Forth dictionaries are
  case-insensitive here, so `CON?`/`VAR?` clash with existing words. Prefix to
  disambiguate (`TYCON?`, `TYVAR?`). When in doubt, `' NAME` in a REPL: if it
  resolves, the name is taken.
- **Do not shadow native primitive names.** Later dictionary entries can replace
  primitive signatures and codegen hooks; `shadow-lint` gates this class of bug.

## Words & factoring

- **Strictly typed Habu, everywhere you can (BLOCKING).** The default is not
  "checked when convenient" — it is checked/typed, and unchecked is only a named,
  tested boundary the checker genuinely cannot express (see *Unchecked code is a
  named boundary*). Think in small, typed words: factor aggressively, give each a
  real `( in -- out )` effect, and compose them into **nice-reading checked DSLs**
  that read as the domain, not as stack plumbing. The three detailed rules below —
  typed by default, small factored words, DSL-first vocabulary — are this one
  principle expanded; if you reach for a giant word, deep juggling, or a raw `s"`
  blob, stop and build the typed words first.
- **Default new public/library Forth to checked typed definitions.** If the
  checker can express the layer, write an explicit typed effect and let `hb`
  verify it, e.g. `: SQUARE ( i64 -- i64 ) dup * ;`.
- **Keep control flow and multi-step computation out of argument lists.** The
  checker *does* accept `if/else` (and comparisons like `f> 0=`) producing a value
  mid-arg-list — e.g. `s" k" 1 0 > if 5 else 6 then 2.0 L-OF` type-checks and runs,
  so this is a readability/safety rule, not a checker limit. Prefer extracting the
  value into a named word with a real effect (`: DET-MINRATE ( -- r ) ... ;`) or a
  pre-bound local, then pass the simple word. Dense arg lists that splice `if/else`,
  comparisons, and several `@`/`F@` reads are where a wrong cell *type* (a `bool`
  from `0 >` where the callee wants `n`) hides — it surfaces at a *later* call as a
  confusing "expected n actual bool", not at the offending column. One value per
  concept; let each line read as the field it emits.
- **Check every source you can — byte emitters included.** Raw byte/layout
  emitters, ELF/Mach-O writers, tooling, tests, and build helpers are checkable
  unless a specific primitive boundary proves otherwise. Verify with the owning
  `bin/hb --load ...` or `tools/check.f --source-list` path before claiming the
  checker cannot express a layer.
- **Unchecked code is a named boundary, not a habit.** Use `0 set-check`,
  raw emitter words, or `TRUST` only for layers the checker cannot express:
  metaprogramming, source-string generators, primitive emitters, snapshot/build
  drivers, and similarly low-level support. Keep the boundary obvious in the
  file and add focused tests for the contract it asserts.
- **Do not stub real facts with `TRUSTED:`.** A predicate, target selector, or
  runtime fact named with `?` must execute a real body that pushes a boolean.
  `TRUSTED:` may assert nominal identity casts or primitive boundaries, but it is
  not a signature-only forward declaration for words owned by another file.
- **Factor reusable helpers back into checked Forth.** If an unchecked harness or
  tool grows a helper that can be typed, move that helper to checked code instead
  of letting unchecked scaffolding become the library surface.
- **Try the checked factor before adding trust.** If existing primitive effects
  can express the operation, define a small typed word and use it from callers;
  add a primitive model or `TRUSTED:` shim only after proving the checker cannot
  certify that helper.
- **Trust the uncheckable operation, not the dispatcher.** For indirect emitter
  callbacks, keep the row/dispatch word checked and isolate only the raw
  `execute` in a tiny `TRUSTED:` shim. Do not convert a whole factored dispatcher
  to trusted just because one leaf operation is higher-order.
- **Build checked task vocabulary before fighting syntax.** If a test, tool, or
  benchmark needs structured rows, JSON/TSV fragments, generated source,
  diagnostics, packets, or repeated assertions, factor domain words or a focused
  checked DSL first. Giant `s"` literals, fragile escaping, and private byte
  emitters are bugs unless they are the tested boundary of that DSL.
- **Readable DSLs execute the body they name.** Prefer forms such as
  `[: ITEM ;] NAME-FILES` or `s" suite-name" TEST-SUITE ... ;TEST-SUITE` over
  generic list wrappers. A generic `execute` layer needs higher-order effects the
  checker may not model; direct row/body words keep the effect visible.
- **Classification tables beat token ladders.** When a word becomes a long
  `dup`/`over` chain over token classes, move the classes into row/table data and
  factor named transition helpers. The tests should describe the table policy,
  not reconstruct a branch ladder.
- **Small, single-purpose words**, aim ≤ 5 lines. A word should read top-to-bottom
  without you tracking more than a few stack items.
- **Raw compiler/emitter code is not exempt.** Unchecked words, register-level
  emitters, and bootstrap/compiler helpers still need exact stack-effect comments
  and small factored helper words. If review requires reconstructing stack state
  from surrounding code or register side effects, factor first; do not rely on
  hand-tracked effects.
- **Factor when the stack gets unreadable.** If you reach for `ROT -ROT PICK
  ROLL`, stop and either factor a helper or use locals. Deep juggling is exactly
  what this project forbids in *user* code — hold our own code to it.
- **Locals `{: a:type b:type :}`** are encouraged where they remove juggling.
  They bind inputs only; do not put `-- outputs` inside the locals form. Keep
  the effect in the stack comment. New locals are typed by default when the
  concrete checker type is known; a bare local name is allowed only when the
  entry stack effect intentionally preserves richer role detail that the local
  annotation cannot express, or when the missing typed capability is documented.
  New diff-introduced bare locals must be made explicit with
  `typed-local-lint: allow-bare-local` so review sees the exception.
- **Local type annotations can erase role detail.** A local such as `a:ptr`
  records only a pointer cell; it does not preserve `ptr u8`. If the body uses
  byte operations such as `c@`/`c!`, keep the detailed type in the stack effect
  and bind an untyped local name, or factor a helper whose entry effect carries
  `( ptr u8 ... -- ... )`.
- **Name same-type numeric slots before reordering them.** Effects such as
  `( cap used add -- )` are all `n`; a stray `swap` type-checks but changes the
  meaning. Bind names at the helper entry or factor role-specific helpers before
  doing capacity, offset, or decoder arithmetic.
- **Bind locals only at a helper entry or before control flow opens.** Do not
  introduce `{:` groups inside an active `if`, `begin`/`while`, `?do`, or after an
  `exit` path. Factor a helper whose inputs can be bound at entry, or use owned
  scratch cells for loop/control state. Mid-control locals can fail in raw
  compilation at `{:` before the checked diagnostic path can explain the bug.
- **Keep locals before live control.** A later `{:` group is valid only while the
  definition is still on a live top-level path; factor another helper instead of
  introducing locals inside control flow or after `exit`.
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
- **Keep physical source lines short.** Long `--load` builders and check source
  appenders must be factored into helper words or split across lines. Do not let
  load-list lines approach the interpreter input buffer; source truncation can
  surface later as unrelated top-level words.
- **Keep script argv explicit.** `hb tool.f arg...` preserves single-script
  compatibility and treats `arg...` as script arguments. Use `--load` only when
  the command line contains more than one source file.

## Stack comments

- **Every definition** carries `( before -- after )`.
- **Checked definitions use type tokens only.** The checker reads the stack
  comment as the signature, so write `( n n -- )`, `( bool -- )`, or
  `( ptr u8 n -- )`, not arbitrary role names such as `( got want -- )`. Standard
  nominal role tokens such as `idx`, `len`, `count`, `fd`, `rc`, `reg`, `label`,
  `va`, `symidx`, `asm`, `img`, and `snap` are real checker types; informal
  names still belong in locals (`{: got want :}`), helper names, or nearby prose.
- **Use real types, not reflexive `n`.** A string is `ptr u8 n`; a dereferenced
  cell address is `ptr a`; a pointer-valued cell should preserve its nested
  pointer role. `n` is only for genuine scalar cells.
- **Same-cell values need nominal roles.** Values with the same runtime
  representation but different contracts (`reg`, `label`, `va`, `symidx`, `fd`,
  `count`, `asm`, `img`, `snap`) get distinct type tokens and negative checker
  fixtures. A raw `( n n -- )` signature hides swaps the checker should reject.
- **Raw role casts are not validators.** Cast words such as `>LEN`, `>IDX`,
  `>COUNT`, `>OFF`, `>ASM`, `>IMG`, and `>SNAP` are trusted identity boundaries.
  Public libraries should expose checked constructors and role-specific helpers
  so length/count/offset/phase swaps fail under `CHECK!`.
- **Unchecked/prose-only comments may name roles** when no checker hook consumes
  the comment, but keep the type shape obvious.
- Add inline `( … )` at non-obvious points inside a longer word so the reader can
  re-anchor mid-definition.
- Use standard notation: `x` cell, `n`/`u` signed/unsigned, `d` double, `c-addr u`
  string, `xt` execution token, `nt` name token, `f`/`bool` flag, `?` for
  maybe-present.

## Checker & type model

- **`CHECK!` is the user contract.** Inference (`CHECK`) proves internal
  consistency; user builds verify the body against the declared `( in -- out )`
  and make rejection fatal. Tests for bad programs must assert build rejection,
  not just runtime failure.
- **Every `TRUST` has a same-change audit row.** Add or update the matching
  `TRUSTED.md` row with effect, reason, and focused tests. Adding lines above a
  trust site drifts the manifest; rerun `trust-lint` and fix exact line numbers
  before commit.
- **Typed booleans are real `bool` values.** Produce true/false with typed
  producers such as `0 0=` and `0 0= 0=` or domain helpers. Do not store raw
  `0`/`-1` into a `ptr bool` cell, and do not compare bools with numeric `=`.
- **Pointer-valued cells use cell-indexed `ptr-field`.** When a typed DATA cell
  or record field stores a pointer, compute the cell slot with `ptr-field` so
  `@`/`!` preserve nested pointer types. The index is a cell slot, not a byte
  offset. Raw fixed-header byte offsets need an explicit trusted boundary or a
  modeled byte-offset primitive.
- **Raw state cells still need typed public effects.** Variables used from
  checked code need explicit `TRUST` rows such as `-- ptr n`, `-- ptr bool`, or
  `-- ptr ptr u8`. Boolean state cells are `ptr bool`, and string-pointer state
  should remain `ptr ptr u8` plus a separate length cell.
- **Path-sensitive control is a checker invariant.** `LEAVE`, `EXIT`, `throw`,
  `die`, and `again` must fold or kill paths according to their declared control
  effect. Divergent path arities are soundness bugs; after a dead path, only
  structural closers (`else`, `then`, `loop`, `+loop`, `repeat`, `again`, `;]`)
  may appear.
- **`RECURSE` uses the declared effect.** Recursive calls apply a fresh copy of
  the current definition's declared signature; keep the raw declared signature
  stable after `CHECK!` so rendered/mutated terms cannot corrupt the scheme.
- **Quotations are xts, not closures.** `[: ... ;]` may not read surrounding
  locals until real closures exist. The checker and compiler must reject local
  references while a quotation is open.
- **Checked `catch` is quotation catch.** Consume success outputs inside the
  quotation (`[: WORD drop ;] catch`) and preserve the exact throw code as data at
  an explicit recovery boundary. Do not widen checked code to arbitrary xt catch.
- **Higher-order signatures publish themselves.** If a checked word with
  quotation effects (`DIP`, `KEEP`, row callbacks) passes `CHECK!`, let it render
  into public signatures; do not keep a `TRUST` row just to pin its scheme.
- **Default new higher-order words to CHECKED — function-passing is a checked
  capability.** The checker verifies a quotation parameter (`[ a a -- bool ]`,
  `[ a -- a ]`, …) executed through a call chain AND inside a `?do`/`begin` loop:
  bind the quotation as an ordinary local, thread it to helpers, and `execute` it.
  A comparator heapsort, map, fold, and filter all check this way. Do NOT reach
  for `0 set-check`/`TRUST` for function-passing by precedent — the older
  `src/core/combinators.f` (MAP/FOLD/EACH) is an unchecked boundary that predates
  this and is *not* a model to copy. Only drop to an unchecked boundary after a
  minimal reproducer proves the checker rejects the specific higher-order shape,
  and file a checker-capability gap (per the Checker-Miss RCA below).
- **New type tokens need a checker-only bootstrap stage.** Old `bin/hb` rejects
  unknown stack-comment tokens before checked source can use them. Add parser,
  renderer, and `CC-*` checker support, refresh the native binary, then use the
  role in `TRUST` rows and checked definitions.
- **Phase tokens must reach the side effect they order.** `asm`, `img`, and
  `snap` phase cells should flow through the final sign/write/header operation,
  not just an early wrapper, so callers cannot skip required build stages.
- **Seal the implicit row under declared inputs.** Row polymorphism must not let a
  body borrow below declared inputs. A stack-preserving trusted effect such as
  `img -- img` or `fd -- fd` must not satisfy final output by binding an implicit
  base row that hides underflow.

## Errors

- **Fallible words `throw` a named code** (defined in `src/config.fs`,
  e.g. `E-MISMATCH`); they never fail silently or return an out-of-band flag in
  place of an error.
- **`catch` only at explicit recovery boundaries**: REPL/CLI wrappers, test
  assertions, and stack-preserving outcome adapters that return the exact throw
  code as data. No `… catch drop`, broad `catch 2drop`, or other masking.
- `unreachable`-style `abort"` only for proven-impossible states, with a message.
- **Interactive/REPL support recovers; builders may exit.** Recoverable
  interactive failures should `throw` into REPL recovery (`?`, rollback, reread).
  Use process exits such as `die` for build-time makers and CLI boundaries where
  terminating the process is the contract.
- **`throw` and `die` are different control effects.** `throw` is catchable and
  belongs to the checker exception edge; `die` terminates the process and belongs
  to no-return metadata. Do not add dummy output values after `throw` to balance
  a branch. If the checker cannot accept a real throw guard, fix the exception
  model or track that capability gap.
- **`die` consumes a real message and code.** Its effect is
  `( ptr u8 n n -- )`: pass an actual byte string and exit code, not `0 0` as a
  fake string. Model process exits as no-return control flow only at certified
  wrappers.

## Constants

- **Named constants, no magic numbers.** Limits and codes live in `src/config.fs`.
  A literal in code is only acceptable for true primitives of the encoding
  (e.g. the `3`/`7` of the 3-bit tag, and even those get a comment).
- **Default to `$hex` literals.** Bit masks, instruction encodings, ASCII codes,
  memory/struct/byte offsets, and field strides are always hex (`$FF and`,
  `$D10043FF`, `$200`, `$40`). Only genuine small decimal *counts* stay decimal:
  loop bounds, arities, shift amounts, and register indices. When in doubt,
  prefer hex. The standalone parses `$hex` (case-insensitive, optional leading
  `-`).

## Testing (BLOCKING)

- **Every word is exercised by `T{ … -> … }T`** as it's written — happy path plus
  each error/edge. A word without a test is unfinished.
- Tests live in the native gate: `test/engine-suite.f`, focused `tools/*-test.f`
  fixtures, and source-specific checks wired through `test/run.f`.
- Assert the **specific** outcome: inside checked definitions use
  `[: WORD ;] TTHROWSQ` or another stack-preserving quotation `catch` and check
  the exact THROW code; top-level scripts that cannot push quotations may use
  `' WORD TTHROWS`. For diagnostics, capture text and match a substring.
- Run focused fixtures during dev with their owning `tools/*-test.f`, then run
  the full native gate command shown in `docs/bootstrap.md`.
- **False-reject claims need execution proof.** Count a checker limitation only
  after running an unchecked copy and proving the measured stack behavior matches
  the declared effect. Generator bugs become rejections, not false certifications.
- **Signature-token changes need direct smoke probes.** Before rebuilding around a
  new token, test the atom parser/type mapper directly (`ATOM-TOK?`, `TOK-TYPE`,
  renderer output) so prefix/length mistakes fail small.

### Checker-Miss RCA

- Treat the phrase "why didn't the checker catch this?" and equivalent wording
  as an immediate trigger, even in meta-discussion about process. Before any
  tool call or visible text, ask: **What static invariant should have made this
  impossible before runtime, and where should the compiler/checker enforce it?**
  The first visible line of the response, progress update, note, dot, or
  investigation is `Static invariant:` followed by that invariant and boundary.
  If the invariant is not known yet, say so on that same line and reduce the
  case until it is known. If you have already started with runtime symptoms,
  stop and restart from this line. Questions about strengthening this rule,
  quoted examples, and process reviews are still live triggers.
- Do not put runtime symptoms first. The checker/compiler/primitive model owns
  the investigation until the exact checked path proves that the invariant is
  outside its contract. Guards, runtime repairs, documentation edits, and
  library edits come only after the static owner and negative regression are
  identified.
- Use this template before editing runtime/library code:
  `Static invariant:` the pre-runtime fact that should be impossible to violate;
  `Owner:` checker semantics, compiler/codegen model, primitive/boundary effect,
  or typed capability gap; `Path proof:` exact command proving the source path is
  fail-closed or the harness gap to fix first; `Reproducer:` minimal checked
  source that should reject; `Compiler fix:` checker/compiler/primitive metadata
  change or capability dot; `Regression:` negative test added with the fix.
- Prove the exact command path is fail-closed before touching runtime/library
  code. If bad checked source can run on that path, fix the harness/tooling path
  first.
- Classify the miss as wrong primitive/boundary effect, checker semantics,
  codegen/runtime mismatch, or same-type semantic-role gap. Add a minimal checked
  reproducer and a negative regression for that class.
- The runtime/library repair is incomplete until the checker/compiler/primitive
  model rejects the bad program. If the checker cannot yet express the invariant,
  create a detailed dot for the missing checker capability and keep only a named,
  tested boundary until that capability lands.
- Treat the compiler/checker as the owner until evidence excludes it. The normal
  fix sequence is: add the minimal negative checked regression, make it fail for
  the right reason, update checker semantics, compiler metadata, primitive
  effects, or boundary typing, then repair any downstream code.

## Comments & hygiene

- `\` line comments, terse. No restating what the code obviously does.
- Remove scratch/debug prints before commit.
- If a definition fails to compile in raw engine mode, Habu reports the undefined
  word on stderr and then may spill the rest of that definition through the
  interpreter. The native `tools/check.f` runner with `--json-errors
  --all-errors` wraps matched undefined tokens in schema-1 JSON diagnostics.

## Habu Native Tooling Gotchas

- **Use the native debugger stack before print-marker probes.** Runtime and
  codegen RCA starts with `docs/debugging.md`: `.s`, `BPW+` watch cells, REPL
  `step`, compiled-word breakpoints (`BP+`, `BP*`, `BPN`), `tools/jitdump.f`,
  and `tools/imgdump.f`. Extend those tools when they cannot expose the needed
  state; do not hide a missing debug surface behind ad hoc prints.
- **Semantic xref is an in-image responsibility.** Dictionary ownership,
  word-reference, and call/reference RCA should use Forth words in the live
  image (`XREF`/`SEE`/`USES`/`USED-BY` or their current equivalents), with any
  CLI as a thin wrapper. Use source search only to locate files or as a temporary
  fallback after verifying the native word is missing; then add a dot for the
  missing Forth capability.
- **Boundary spawns must attribute failures.** Gate/test/tool boundaries that
  spawn `hb` or another child use outcome capture for expected timeouts and
  failures, not throw-only capture that collapses into a shell rc. The failure
  report must include the suite/case label, phase, executable and argv/load list,
  outcome kind/code, named rc when known, capture bytes/capacity, and captured
  stdout/stderr. Use throw-on-timeout capture only inside a focused unit test
  whose assertion is the named throw itself.
- **Large native tool bundles are supported.** Do not split tools merely to dodge
  DATA pressure. `create ... allot` is for dictionary-sized static storage; large
  runtime-sized buffers use `lib/memory.f` (`MEM-ALLOC-BYTES` or
  `MEM-ALLOC-64K-BUFFERS`) so composition scales with OS-backed mappings rather
  than `DATA-SIZE`. Tools may keep as many 64K buffers and live spans as their
  workload needs, either as one contiguous `MEM-ALLOC-64K-BUFFERS` span or as
  many independent spans. The only accepted limits are cell-size overflow checks
  and an explicit OS allocation failure. If ordinary composition still hits
  capacity, fix the shared memory model and add a regression for the composed
  load.
- **Missing convenience words are not bugs in the standard.** Habu currently lacks
  words such as `pick`, `within`, and `0<>`; use variables, explicit increments,
  or explicit comparisons (`0 = 0=` for nonzero).
- **Trust is audited, not permanent.** `TRUST` records asserted effects so callers
  can be checked, but audit rows must stay current and stale dates must fail lint.
- **Typed pointer fields use cell indexes.** When a variable or record cell
  stores a pointer, construct a `ptr ptr x` field with `ptr-field`, then use
  normal `@`/`!`. Do not multiply indexes by cell size before `ptr-field`; use a
  named trusted boundary for raw byte-offset header cells.
- **Keep `TRUSTED:` bodies syntax-simple.** Do not use locals inside a trusted
  body. Factor checked helper words for real work, then keep the trusted body to
  the minimal operation that the checker cannot express.
- **Checked tool libraries restore checking.** A shared lint/check/tool library
  must not leave callers in unchecked mode. Declare and test any boundary
  locally, then reinstall `CHECK!` immediately after the raw declarations.
- **Generated unchecked spans are split at the first checkable file.** When a
  build tool emits `0 set-check`, prove the shortest source span empirically,
  reinstall the hook as soon as the next file checks, and pin the cut with a
  source-shape regression.
- **Generated checker preludes must rebind `HOOK`.** If generated source reloads
  `src/core/checker.f` or `src/core/render.f`, it must reload
  `src/core/check-hook.f` before `' HOOK set-check`; otherwise the hook can still
  call the old `CHECK!`.
- **Bootstrap/fixpoint temp roots are explicit script args.** Stage2/fixpoint
  sources must not depend on stale seed envp capture. Pass the temp root after
  `--`, keep all generated paths under that root, and let the build driver own
  path construction.
- **Generated strings use byte writers for syntax.** Habu `s"` literals do not
  escape embedded quotes. JSON, source needles, and rows with quoting should be
  built with checked byte/field helpers or `lib/json-write.f`, not host encoders
  or fragile escaped string literals.
- **Source-use guards match tokens, not substrings.** Required-word checks and
  boundary scans must lex whole tokens and skip comments/strings; substring
  matches create false positives (`FOO` matching `FOO-BAR`) and hide policy bugs.
- **Preflight unchecked native emitters.** Raw image/primitive emitters still
  need checked shape tests before `BUILD-IMAGE`: no mid-control locals, no second
  locals groups, no hand-balanced descriptor math. Use named scratch cells and
  small helpers, then gate the forbidden source shapes in `tools/build-fixpoint.f`
  so bad emitters fail before a snapshot or `bin/hb` candidate is written.
- **Emitter punctuation is semantic.** Words such as `BL,`, `LBL,`, `ADR,`, and
  `ZBYTES,` are distinct from punctuation-less names; source-shape regressions
  should assert exact emitted tokens. Emitter stack comments describe the
  host/build-time stack (`( -- )`, `( n -- )`); document emitted runtime effects
  in nearby prose or in the generated word's own contract.

## Native Forth Gotchas That Shape How We Write Code

(Build/environment findings are in `../LESSONS.md`; these are the ones that affect
*coding*.)

- **Case-insensitive** dictionary -> name-collision risk (see Naming).
- **`[']` is compile-only.** In interpreted tests, use `'` (tick) to get an xt,
  e.g. `' WORD catch`.
- **Control words and ticks are compile-only** — `if`/`else`/`then`,
  `begin`/`while`/`repeat`, `[']`, `i`, `?do`, and `;` must live inside a
  `:` definition, never at the top level.
- **A `begin <cond> while <body> repeat` condition may only *add* a flag.** The
  stack below the flag at `while` must equal the stack at `begin`; a condition
  that net-produces carry values (e.g. `a u NEXT-TOKEN` leaving a token span
  under the flag) is rejected at `repeat`. Establish loop-carried values *before*
  `begin` (they thread through unchanged), or move the production into the body —
  a peek-only flag condition plus an extract-in-body step.
- **A no-`else` `if` must be stack-neutral.** If the true branch changes stack
  depth the merge at `then` fails (`expected: … actual:`). Bind the consumed
  value into a local *before* the `if` so both paths balance, or add `else drop`.
- **A local may not be bound after an `exit` has appeared on a path.** Binding
  after a *closed* `if`/`else` with no early `exit` is fine; the blocker is
  specifically a prior early-return guard. Put all `{:` groups ahead of the first
  guard, or factor the post-guard work into a helper that binds at its entry.
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

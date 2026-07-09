# Type Families And ADTs Plan

## Goal

Implement the generic type-family and algebraic-data-type system from
`docs/type-families.md`: registered parametric families, layout-bearing sums,
enums, products, generated constructors, checked exhaustive `MATCH`, runtime tag
validation, layout-aware stack operations, logical diagnostics, and tests. This
replaces the discarded one-off `Result` branch; no Result-specific checker
feature is allowed.

## Constraints

- Implement in checked Habu wherever the checker can express the code.
- Keep unchecked code to named, tested boundaries with `TRUSTED.md`,
  trusted-inventory, and checked-boundary-lint coverage.
- Organize new public APIs in packages. Registry records store package id,
  public/private visibility, and family tail; exported constructor words use
  package public names such as `RESULT:OK` for a top-level `result`, not
  unqualified dotted globals.
- Type tokens in signatures keep the existing type-token convention
  (`span<...>`, `result<...>`), and type-family names are lowercase because
  signature types are system vocabulary. Declarations and signature references
  reject uppercase or mixed-case family names (`Result`, `RESULT<n>`) instead of
  folding them. Callable words remain project words, so generated constructors
  use uppercase package APIs such as `RESULT:OK`. Type-family lookup is
  package-aware and qualifier-aware.
- Logical multi-cell values are never modeled as one opaque cell.
- Hidden physical fields beginning with `@` are checker-owned and cannot appear
  in public signatures or public-signature manifests.
- System implementation registries are not ordinary package APIs. User source
  must not call or `undefine` registry mutators, rollback hooks, constructor
  publish hooks, or sealed implementation entries.
- `MATCH` is a compiler/checker control form, not a quotation combinator.
- Runtime `MATCH` checks tags and dies on invalid tags even for checked code.
- Binary size must not grow materially without a byte-map explanation.

## Package Shape

- `package TFAM`: checker/runtime registry internals for families, variants,
  schema nodes, layout records, rollback, snapshot persistence, and lookup.
  Registry mutators, constructor publish hooks, rollback hooks, and raw arena
  helpers are private implementation entries reached only through a sealed
  system/friend capability from the language parser and checker. They are not
  exported as ordinary callable `TFAM:*` words. Read-only query words may be
  exported only if they cannot mutate registry state or bypass visibility.
  `TFAM`, `TYPE`, and `MATCH` are sealed system packages, not ordinary
  reopenable user packages, because package-private helpers are shared across
  reopened blocks by design.
  Sealing is enforced at the wordlist layer, not only at `package NAME`: raw
  `set-current`, `get-current`, `search-wl`, `parse-name`, `'`, `[']`,
  `execute`, `postpone`, `compile,`, `wordlist`, `XREF-START`, `XREF-LEN`, `XREF`,
  `LATEST`, raw record readers,
  `CHECKER-PACKAGE`, `CHECKER-UNDEFINE`, `CHECKER-DEFTYPE`,
  `CHECKER-DEFLINEAR`, `CHECKER-DEFRECORD`, `CHECKER-DEFER`, lifecycle
  truncation, direct dictionary memory writes (`dbase@`, `data-base`, `patch32`,
  `!`, `c!`, `+!`, `atomic!`, `atomic-cas`, `here`/`allot`/`,`/`c,`), raw
  code-pointer exposure (`cp@`/`rbase`), `immediate`, and low-level XREF
  retirement words, and qualified-definition paths cannot publish into, delete
  from, read/execute/postpone/compile from, or expose mutable handles for
  sealed system WIDs or generated constructor metadata WIDs. Public generated
  constructor packages are closed but callable: users may look up, execute,
  postpone, and compile the published constructor words such as `RESULT:OK`,
  but may not publish extra tails, delete entries, expose mutable handles, or
  reach private metadata through that package. The same
  guards apply case-insensitively and in the native, `habu1`, and Gforth
  bootstrap mirrors.
  Protected wordlists and checker/dictionary memory ranges carry provenance:
  raw stores, atomics, arena writes, every primitive/syscall effect with a
  writable pointer (`read`, `readlink`, `stat64`, `lstat64`, `getdirentries64`,
  `poll`, `ioctl`, future writer syscalls), `mmap` remapping, and `ffi-call*`
  pointer arguments cannot target protected regions without the sealed friend
  capability. AOT image restore must persist the protected-WID registry, restore
  WIDs without u8 truncation, and advance `WIDN` above every restored protected
  WID before any user `wordlist` or `package` allocation can run.
- `package TYPE`: user-facing definers that are ordinary language words only at
  the parser boundary (`TYPEFAMILY`, `SUMTYPE`, `VARIANT`, `;VARIANT`,
  `;SUMTYPE`, later `ENUM`/`PRODUCT`). Parser keywords may be global/reserved, but their
  implementations call TFAM through the sealed system/friend capability rather
  than user-callable package words or direct arena access.
- `package MATCH`: checker/compiler support for `MATCH` frames and variant
  refinement. Public parser words are reserved language tokens; implementation
  state is private to the package.
- Generated constructor packages are keyed by the resolved `family-id`, not by
  a bare family tail. A top-level `SUMTYPE result` keeps the logical type
  spelled `result<...>` and publishes callable words `RESULT:OK` and
  `RESULT:ERR`. If package `PKG` also defines a same-tail `result`, callers use
  a legal one-colon generated package spelling; `PKG:RESULT:OK` is invalid.
  The constructor package tail is injectively derived from stable source
  identity by one pinned algorithm shared byte-identically by native `habu2`,
  `habu1`, and the Gforth bootstrap mirror: uppercase each canonical package
  path segment and the family tail, escape every literal `-` inside every
  joined segment — the family tail included — as `--`, and join segments plus
  tail with single `-` separators (tail escaping is required for injectivity:
  package `a` + family `b-c` must not collide with top-level `a-b-c`); if the
  escaped spelling exceeds the engine dictionary name-length limit, the
  spelling is `T` plus the first 16 lowercase hex digits of SHA-256 over the
  length-prefixed unescaped segment list, then `-` plus the raw uppercase
  family tail. It never uses
  allocation-order numeric package/family ids for visible spelling. The derived
  package is reserved/non-reopenable as an ordinary package and is
  collision-checked against existing package names and qualified definition
  auto-created wordlists so it cannot collide with the top-level `RESULT`
  package or any user-created package/qualified wordlist with the derived name.
  Rendered type tails stay lowercase; same-tail package identity appears in
  machine-readable diagnostics and in human text only when needed to
  disambiguate. Payload/layout helpers and hidden physical fields remain private
  checker metadata, never public words.
  Private family constructors are not ordinary dictionary words. After item 9
  installs the constructor token protocol, they use a checker-owned constructor
  form:

  ```forth
  construct family variant
  ```

  The parser consumes both tokens, resolves them to `(owning-package-id,
  family-id, variant-id)` while the owning package is open, and records that
  tuple in checker/compiler capture. Public families may additionally publish
  external constructor package words. Private families do not export external
  packages, do not create bare variant words, and cannot collide with another
  private family that uses the same variant tail.
- Existing broad globals such as `VALUE-RECORD`, `ENUM`, and structure helpers
  are migration surfaces. New helper words should not add another prefix-stemmed
  global API when a package-private helper or package public export is possible.
- Implementation files must follow package boundaries instead of growing
  `src/core/checker.f` indefinitely. New registries/control systems land in
  package-owned files such as `src/core/type-family.f`,
  `src/core/type-family-effects.f`, `src/core/type-schema.f`,
  `src/core/sumtype.f`, `src/core/match.f`, and matching tests, with
  `tools/srclist.f`, `FILEMAP.md`, build-cache keys, and bootstrap source lists
  updated in the same item. Existing `checker.f` call sites may get narrow
  integration hooks only.

## Current Grounding

This plan cites files and word/symbol names, not line numbers; per-item
`file:line` maps live in `docs/census-tfam-*.md`, refreshed by scouts.

- Spec: `docs/type-families.md`.
- Parametric terms: `src/core/checker.f`.
- Current parametric terms store constructor spelling and compare names at
  `src/core/checker.f`; registered type-family terms must carry resolved
  `family-id`, with source spelling kept only for diagnostics.
- Param/effect replay still rebuilds terms from strings at
  `src/core/checker.f`; those copy/instantiate paths must preserve `family-id`
  too.
- Hard-coded family whitelist to remove: `src/core/checker.f`.
- Value-record registry and expansion: `src/core/checker.f`,
  `src/core/roles.f`, `test/engine-suite.f`, `lib/ptx/ir.f`.
- Candidate/scope rollback: `src/core/checker.f`.
- Source preverify: `src/habu/verify-source.f`, `tools/check-core.f`.
- Package/defer state is also mutable during preverify and all-errors replay:
  `src/habu/verify-source.f`, `tools/check-all-errors-core.f`.
- Control-flow checker frames: `src/core/checker.f`.
- Compiler keyword/control lowering: `src/habu/habu2.f`.
- Top-level definers and stack introspection can consume or expose physical
  cells: `src/core/checker.f`, `src/habu/habu2.f`, `src/habu/habu1.f`,
  `bootstrap/cg/forth.fs`.
- Gforth bootstrap mirror and shape tests: `bootstrap/cg/forth.fs`,
  `tools/compiler-dispatch-test.f`, `tools/bootstrap-codegen-test.f`.
- Structures/enums surface: `src/core/structures.f`,
  `src/core/structures-effects.f`, `src/core/enums.f`,
  `test/gate-dictionary-lib.f`.
- Diagnostics/rendering/API manifests: `src/core/render.f`,
  `tools/repair-schema-doc-test.f`, `tools/repair-packet-core.f`,
  `tools/gate-json-assert-core.f`, `tools/public-signatures-core.f`.
- Trust and policy tools: `TRUSTED.md`, `tools/trusted-inventory.f`,
  `tools/checked-boundary-lint-core.f`, `tools/trust-lint-core.f`.
- Build cache/source lists: `tools/hb-build-lib.f`.
- Gate result-cache/source-list roots: `test/run-files.f`, `tools/srclist.f`.
- Redefinition/undefine protection: `src/habu/xref.f`, `src/core/checker.f`.
- Raw wordlist and package mutation: `src/core/checker.f`, `src/habu/xref.f`,
  `src/habu/habu2.f`, `src/habu/habu1.f`, `bootstrap/cg/forth.fs`.
- AOT restore and wordlist allocation can reuse raw WIDs if not advanced after
  replay: `src/habu/habu2.f` (`EM-AOT-REGISTER-RECS`,
  `EM-STARTUP-RUNTIME-STATE`, `C-PACKAGE-ALLOC-WIDS`), `src/habu/habu1.f`
  (`BWORDLIST`), `src/habu/aot-capture.f`.
- Syscall/FFI writer boundaries can mutate through pointers outside ordinary
  store words: `src/core/checker.f`, `src/habu/habu1.f`,
  `bootstrap/cg/forth.fs`.
- Execution sinks that can compile or run protected words by xt:
  postpone/compile, sinks `src/habu/habu2.f` (`C-POSTPONE`),
  `src/habu/habu1.f` (`BCOMPILE`), `bootstrap/cg/forth.fs`; execute sinks
  `src/habu/habu1.f` (`BEXEC`), `bootstrap/cg/forth.fs`, checker `RSEXEC`
  `src/core/checker.f`.
- AOT/object test entries and cache keys currently assume the normal `MAIN`
  entry unless extended: `src/habu/aot-closure.f`, `src/habu/aot-lib.f`,
  `tools/hb-build-lib.f`, `lib/object.f`, `lib/object-resolve.f`,
  `lib/object-index.f`, `lib/object-cache.f`, `lib/object-link.f`,
  `tools/object-image.f`.

## Implementation Items

1. **Install the spec and retire the discarded branch**
   - Paths: `docs/type-families.md`, `PLAN.md`.
   - Work: keep the moved design doc as the normative input and ensure the
     reverted result-type work leaves no source, doc, filemap, or trust rows
     except this generic design. Update `docs/type-families.md` examples and
     test snippets to use lowercase type names (`result<...>`) and package
     constructor words (`RESULT:OK`/`RESULT:ERR`) instead of dotted constructor
     names. Pin the generated constructor package derivation at
     `docs/type-families.md` to the exact escape/hash encoding in
     Package Shape, and normalize every `VARIANT` example to the terminated
     `VARIANT ... ;VARIANT` form so the spec grammar is uniform.
     Add `docs/type-families.md` to `FILEMAP.md` so the normative spec is
     discoverable and protected by filemap-lint. Rebuild/refresh `bin/hb` on
     this line so its baked source list no longer references discarded
     `src/core/result.f`; stale `bin/hb` is a blocking failure, not an
     acceptable risk.
   - Acceptance: `rg` over source/docs excluding `PLAN.md` and
     `docs/type-families.md` finds no `RESULT:`, `result<`, `src/core/result`,
     `MMAP>BYTES`, `MMAP>CELLS`, or `MAP-ARENA`; `jj log` shows the result
     branch reverted on this line; `tools/filemap-lint.f` covers
     `docs/type-families.md`; `printf '' | bin/hb --load tools/filemap-lint.f`
     passes and no longer tries to open `src/core/result.f`.
   - Risk: rebuilding from a stale binary may require a bootstrap/refresh path
     before source-only validation can run.
   - Effort: small.
   - Depends on: none.
   - Goal mapping: prevents one-off Result work from leaking into the generic
     type-family implementation.

2. **Add package-scoped `TFAM`, `SUMV`, `SCHEMA`, and layout registries**
   - Paths: `src/core/checker.f`, `src/habu/habu2.f`, `docs/type-families.md`.
   - Work: add growable checker-owned registries for families, variants,
     product fields, logical layouts, and persistent type schemas. Records store
     package id, public/private visibility, canonical lowercase tail name,
     arity, kind, parameter kinds, layout policy, slot count, variant range, tag
     width, schema roots, and source span. The declaration parser rejects
     uppercase/mixed-case type-family tokens before storing the canonical tail;
     the registry must not normalize `Result` into `result`. Reuse the
     grow/rebase/snapshot pattern, but split the registries into package-scoped
     helper words instead of enlarging VREC. Add sealed-system-package support
     for `TFAM`, `TYPE`, and `MATCH` so user source cannot reopen their private
     wordlists or publish into their internals. Add an explicit source-origin
     friend capability implemented as a boot latch: the engine loader sets the
     friend flag while loading the engine's canonical baked source list (the
     `tools/srclist.f` order used by the native build, `habu1`, and the Gforth
     bootstrap mirror) and permanently seals it before any user source is
     evaluated (`--load` files, stdin, REPL, `evaluate`; there is no engine
     `--source-list` flag — user-supplied source-list files reach the engine as
     materialized loaders through `--load` and are covered by that path). No
     user-supplied source-list file is friend-origin. Sealed
     package creation/reopen and every friend-only guard in this plan read that
     latch, and the same `package TFAM`/`package TYPE`/`package MATCH` token
     from user source rejects before mutation. Persist protected-WID metadata through AOT
     seed capture/restore with a widened WID field, not the current u8
     compaction, and restore `WIDN` above the maximum restored WID before user
     allocation resumes.
   - Acceptance: fixtures prove add/find, qualified and unqualified package
     lookup, public/private isolation, duplicate rejection within a package,
     same tail allowed across different packages, arity lookup, kind predicates,
     grow across every initial cap, malformed schema-node rejection, snapshot
     persist/restore, uppercase/mixed-case type-name rejection, and no
     hidden-field lookup from public signatures. Attempts to `package TFAM`,
     `package TYPE`, `package MATCH`, or define qualified words inside those
     system namespaces from user source reject fail-closed. Attempts to call or
     `undefine` system mutators such as `TFAM:ADD`, constructor publish hooks,
     rollback hooks, or sealed `TYPE`/`MATCH` implementation entries from user
     source reject fail-closed; internal parser/checker calls still work through
     the sealed system/friend path. Attempts to reach sealed or generated WIDs
     through raw `set-current`, `get-current`, `search-wl`, `parse-name`,
     `'`, `[']`, `execute`, `wordlist`,
     `XREF-START`, `XREF-LEN`, `XREF`, `LATEST`, raw record readers,
     `XREF-WORDLIST`, `CHECKER-PACKAGE`/`CHECKER-PUBLIC`,
     `CHECKER-UNDEFINE`, `CHECKER-DEFTYPE`, `CHECKER-DEFLINEAR`,
     `CHECKER-DEFRECORD`, `CHECKER-DEFER`, direct dictionary memory writes
     (`dbase@`, `data-base`, `patch32`, `!`, `c!`, `+!`, `atomic!`,
     `atomic-add`, `atomic-cas`, `here`/`allot`/`,`/`c,`), raw code-pointer
     exposure (`cp@`/`rbase`), `immediate`/`DNAME-IMM`, `cp!`, `ndict!`,
     `CHECKER-USIGS-TRUNCATE-FROM`, `XREF-RETIRE`, `XREF-RETIRE-WL`,
     `UNDEFINE-NAME`, `UNDEFINE-FOUND`, `UNDEFINE-IF-DEFINED`,
     `HIDE-DEFS-FROM`, `FORGET-DEFS-FROM`, `postpone`, `compile,`, or xt-based
     execution paths reject or are friend-only before mutation, compilation,
     execution, or visibility lookup. Public generated constructor words are the
     exception: published entries such as `RESULT:OK` remain visible and
     executable/compilable as ordinary public APIs, while their package stays
     closed to extra definitions, deletion, mutable handle exposure, and private
     metadata lookup. Raw syscall/FFI writers from the complete primitive-effect
     census (`read`, `readlink`, `stat64`, `lstat64`, `getdirentries64`, `poll`,
     `ioctl`, `mmap` remap/protection changes, every future writable-buffer
     syscall, and `ffi-call*` pointer arguments) reject any protected
     checker/dictionary/generated-WID pointer provenance unless the caller holds
     the sealed friend capability. Ordinary memory stores to
     non-protected buffers still work, but leaked pointers from `dbase@`,
     `data-base`, WID handles, record readers, code pointers, or generated
     package metadata cannot be written through `!`, `c!`, `+!`, atomics,
     syscalls, FFI, `here`/`allot`/`,`/`c,`, or raw arena helpers.
     AOT seed/restore persists protected-WID metadata, rejects sealed/generated
     WIDs during direct record registration, relocation lookup, and bootrun, and
     advances `WIDN` past the maximum restored WID before any post-restore
     `wordlist` or `package` allocation. AOT seed fixtures create
     protected/generated WIDs above 255 and prove capture/restore preserves full
     WID values without u8 truncation. `snap-rebase` is friend-only or rejects
     protected dictionary/code ranges before rewriting pointers or call-literal
     instruction words. Generated constructor package WIDs are closed namespaces: users
     cannot publish any extra tail such as `RESULT:BOGUS`, not just duplicate
     `RESULT:OK`. Case aliases (`tfam`, `Tfam`, `result:bogus`) reject exactly
     like canonical spelling. The same protection is implemented and tested in
     native, `habu1`, and Gforth bootstrap paths. Internal core/native/Gforth
     source-origin fixtures prove sealed package creation/reopen succeeds only
     through the friend path; user fixtures prove the same spelling rejects.
     Latch fixtures prove the friend flag is sealed before the first user token
     is evaluated and cannot be re-set afterward from `--load`, stdin, REPL,
     `evaluate`, or materialized source-list loader input.
   - Risk: checker arena relocation bugs; every name pointer/string offset must
     rebase correctly.
   - Effort: large.
   - Depends on: item 1.
   - Goal mapping: replaces hard-coded constructor knowledge with package-aware
     metadata.

3. **Make registry rollback transactional and reentrant**
   - Paths: `src/core/checker.f`, `src/habu/verify-source.f`,
     `tools/check-all-errors-core.f`, `docs/type-families.md`,
     `test/engine-suite.f`.
   - Work: add `TFAM`, `SUMV`, `SCHEMA`, product-field, layout, and string-pool
     high-water marks to a rollback-frame stack used by
     `CHECKER-SCOPE-START/DONE` and `CHECK-CANDIDATE-START/DONE`. Roll back both
     successful candidate probes and rejecting scoped loads so failed family
     declarations cannot poison later checks. Existing single-slot rollback state
     must become depth-safe before new registries are added. The frame also saves
     active package mode/name, defer metadata (`DFER`), deferred target caches,
     and any package-local signature indexes that can be mutated by preverify or
     all-errors replay.
   - Acceptance: first checkpoint proves generic rollback-frame depth and
     high-water restore for `TFAM`, `SUMV`, `SCHEMA`, product-field, layout,
     string-pool, existing VREC/CT/SYM/LIN/USIG, package scope, and DFER state.
     As items 6, 8, 14, and 15 land, add bad `SUMTYPE`, constructor, `ENUM`, and
     `PRODUCT` fixtures that reject in candidate/scope and prove no family,
     variant, constructor, package, or defer row remains visible afterward.
     Nested all-errors candidate/scope tests prove parent rollback frames are not
     overwritten.
   - Risk: rolling back hash indexes must retire index entries as well as counters.
   - Effort: medium.
   - Depends on: item 2.
   - Goal mapping: makes type-family metadata safe in checker candidates and
     source-list preverification.

4. **Replace `PARAM-CTOR?` and repair nested param parsing**
   - Paths: `src/core/checker.f`, `src/core/render.f`,
     `docs/type-families.md`, `lib/ptx/*.f`, `docs/census-tfam-4.md`.
   - Work: parse `family<...>` and zero-arity bare family tokens only through
     package-aware internal TFAM lookup; bare `color` must resolve to a family
     id before
     falling through to nominal/builtin lookup. Register current PTX cell
     families (`span`, `matrix`, `gridctx`, `rowctx`, `tile`, `uniform`, etc.)
     during core/prelude load. Replace the single global `PARAM-SCR` parse
     scratch and fixed four-argument storage with recursive-safe growable schema
     arg lists so nested signatures cannot corrupt outer arguments and family
     arity is not capped by `PARAM-MAX-ARGS`. Parsed `T-PARAM` records store
     resolved `family-id` plus child type offsets; spelling and qualifier text
     are retained for diagnostics only. Unification compares `family-id`, not
     folded source spelling, so two packages may define the same lowercase
     family tail without aliasing. Schema nodes include quotation payload
     schemas (`SC-QUOT`) with effect rows and nested family ids; parsing,
     persistence, instantiation, copying, rendering, and diagnostics preserve
     those rows rather than collapsing them to strings. Value-record expansion,
     schema instantiation, effect-node copying, and any replay path that
     currently copies/rebuilds `T-PARAM` by source string must preserve
     `family-id`.
   - Acceptance: existing PTX positives pass; unknown families, wrong arity,
     bad delimiters, nested malformed params, and layout-in-cell-only params
     reject with family-specific diagnostics. Arity greater than the old
     `PARAM-MAX-ARGS` parses through growable storage and either succeeds or
     fails for semantic reasons, never because of a four-slot cap. Nested cases
     such as `foo<bar<n>,n>` parse and round-trip. Same-tail zero-arity and
     parametric families in two packages reject cross-package unification while
     same-family qualified and unqualified references unify. VREC/schema/effect
     replay cannot re-alias same-tail families. `PARAM-CTOR?` no longer contains
     a whitelist. Quotation payload fixtures prove `SC-QUOT` parse, persist,
     instantiate, copy, render, and reject malformed nested effect rows.
   - Risk: preverify and runtime child must receive identical family
     registrations.
   - Effort: large.
   - Depends on: items 2 and 3.
   - Goal mapping: establishes generic parametric type families before ADTs.

5. **Update source preverify, check tools, and policy lints**
   - Paths: `src/habu/verify-source.f`, `tools/check-core.f`,
     `tools/check-all-errors-core.f`, `tools/reserved-name-lint-core.f`,
     `tools/public-signatures-core.f`, `tools/public-signatures-test.f`,
     `src/core/include.f`, `src/habu/habu2.f`, `lib/source.f`,
     `lib/source-test.f`, `test/run-result-cache-test.f`.
   - Work: first implement the generic ordered event log, restricted discovery
     pass, source-span capture, replay engine, and existing support-form parity
     without requiring ADT declarations. After item 6 and item 8 land, plug
     `TYPEFAMILY`/`SUMTYPE` and generated constructor metadata into the same
     event/replay framework so later signatures are checked against identical
     support state. Replace path-set dependency closure with ordered source-composition event
     replay. Every `require`, `required`, `include`, `included`,
     `s" ..." required`, `s" ..." included`, escaped string forms
     (`S\" ...\" required` / `S\" ...\" included`), and `provided` event records
     its kind, exact path string, source span, package/checker state delta, and
     multiplicity. Unsupported string openers such as `C\"` or `.\"` before a
     loader word reject fail-closed instead of being replayed as a different
     source string. `include` events replay every occurrence; `require` and
     `provided` replay exact-string registry state without collapsing distinct
     spellings or materializing everything as `required`. Events are collected
     by instrumenting the runtime include/require/provided words in
     `src/core/include.f`, not by static scanning alone, so stack-string
     `included`/`required` calls are either recorded after evaluation or reject
     fail-closed when a tool requires a static closure and no event artifact is
     available. Tools that currently need closure data before running a child
     (`tools/check.f` static lints, preverify, all-errors, public-signatures,
     result-cache closure) first run a restricted discovery pass that executes
     only source-composition and support-declaration forms and emits the ordered
     event artifact; if that artifact cannot be produced before the consumer
     phase, the consumer rejects fail-closed rather than guessing. `hb-build`
     cache/key generation is an event consumer too: AOT/REPL/object cache keys
     include the ordered replay closure, not only the top-level `HBB-SRC$`.
     Discovery runs against a fresh or snapshotted target-equivalent
     require/provided registry so tool-preloaded paths cannot hide user support
     files. If source redefines, undefines, or hides loader words
     (`include`/`included`/`require`/`required`/`provided`) before closure
     discovery is complete, discovery and cache closure reject fail-closed.
     Add interpreter/current-token source-span capture so event instrumentation
     can record the loader token span. Stack-string loader forms record the
     loader word's call-site span plus a path-origin classification; if a tool
     requires byte-exact path-expression spans and only a dynamic stack value is
     available, it rejects fail-closed.
     Preserve existing source-local support forms in that replay path too:
     `deftype`, `deflinear`, `VALUE-RECORD`, `defer`, `constant`, `create`,
     `variable`, `immediate`, `TRUSTED:`, `TRUST`, `undefine`, `EXPORT`,
     package scope, and any deferred target metadata that later definitions
     need. `--all-errors --source-list` must redrive each original source-list
     file rather than only the materialized temp loader, replaying all prior
     source-list entries before checking a later file so cross-file prefix state
     is preserved. Package
     `public`/`private`/`end-package` state is part of the pre-scan/replay
     environment so package-local families are registered in the right scope.
     Invalid top-level family declarations and bad support signatures involving
     family types are collected as diagnostic units, not treated as unguarded
     support replay. Reserve every new definer/control token at the item that
     introduces it: item 6 reserves `TYPEFAMILY`, `SUMTYPE`, `VARIANT`,
     `;VARIANT`, `;SUMTYPE`; item 9 reserves `construct`, `MATCH`,
     `;MATCH`, plus branch tokens used by `MATCH`; item 14 reserves/migrates
     `ENUM`/`;ENUM`; item 15 reserves `PRODUCT`/`FIELD`/`;PRODUCT`; item
     16 reserves `POLICY`. Do not reserve `ENUM` before the legacy enum surface
     is retired or migrated. Likewise, do not reserve `construct` before item 9
     migrates the pre-existing `CONSTRUCT` words (`lib/task.f`, and their call
     sites), nor `FIELD` before item 15 migrates the pre-existing `FIELD` words
     (`lib/object.f`, `lib/object-test.f`, `src/habu/aot-lib.f`); dictionary
     lookup is
     case-folded, so lower/upper spellings collide.
     Public-signature output is
     synthesized from TFAM/SUMV metadata for generated constructors; it does not
     rely only on source `:` rows. Tool preverify, all-errors support replay,
     public-signature extraction, and repair-schema generation share the same
     ordered source-composition event log so package/family metadata is loaded
     before metadata-derived signatures and `EXPORT` rows are rendered. Tool preverify,
     all-errors support replay, and public-signature extraction must stop
     hardcoding `constant` as `-- a` for layout values; they either reject
     layout constants consistently or carry the full logical value shape.
     Checked-boundary source-list lint must match trust-lint's original-entry
     behavior: source-list mode scans each original input plus discovered event
     dependencies, not only the generated `required` loader.
     Replayed path strings must be encoded through a single checked string
     emitter shared by `tools/check-core.f`, `lib/source.f`, and source-list
     materializers, or rejected fail-closed; raw `s" ..."` materialization must
     not be used for paths containing quotes, backslashes, newlines, or other
     tokens that can change source structure. Diagnostic prefix labels such as
     `DIAG-FILE!` use the same emitter/rejection policy and cannot keep a
     separate quote-only check.
   - Acceptance: first checkpoint proves the generic event log and redrive
     preserve earlier source-list files, ordered source-composition events, and
     existing support forms without any ADT grammar enabled. Repeated-include,
     exact-string require/provided, same-path-different-spelling,
     stack-string `included`/`required`, `provided`, `EXPORT`, `constant`,
     `create`, `variable`, `immediate`, `TRUSTED:`, `TRUST`, and `undefine`
     fixtures prove replay matches runtime source composition and existing
     non-colon support forms survive isolated redrive. Event-closure lints scan
     every discovered include/require/provided dependency path with original
     labels, not only explicit source-list inputs. After item 6, same-file
     `TYPEFAMILY`/`SUMTYPE` definitions can be used by later signatures in the
     same source list, bad family declarations report original file spans, and
     defining/shadowing item-6 tokens fails with reserved-name diagnostics.
     Support-only/no-colon bad declaration files produce first-class
     declaration diagnostic rows instead of being swallowed by colon-definition
     redrive.
     After item 8, public-signature manifests contain exported logical
     constructors and no hidden `@...` fields. After items 9, 14, 15, and 16,
     the matching reserved-token fixtures are added with those items.
     Replayed path fixtures cover `s"` and `S\"` forms, quotes, backslashes,
     newlines, and whitespace, proving fail-closed rejection or byte-exact
     escaping in check-tool replay, `lib/source.f` materialization, and
     diagnostic prefix generation.
     `C\"`/`.\"` loader-form fixtures reject fail-closed. As each ADT item lands,
     bad declarations/support signatures produce diagnostics while later
     definitions are still checked, reserved-name fixtures cover that item's new
     tokens, and public-signature fixtures prove generated metadata rows are
     emitted without hidden fields, including metadata introduced by
     required/included support files and package-reopened source lists.
     Layout-producing `constant` fixtures prove
     verify-source, all-errors replay, and public-signature paths do not treat
     layout constants as one-cell `-- a`. Checked-boundary source-list fixtures
     place a forbidden boundary only in an original input file, not in the
     materialized loader, and prove the lint reports that original file.
   - Risk: source-only trust, signature, reserved-name, and checked-boundary
     lints must continue to scan original source-list input paths, not
     flattened temp sources. As item 9 lands, fixtures must cover `construct`,
     `MATCH`, and new branch tokens in source-list files.
   - Effort: medium.
   - Depends on: items 2-4 for the generic event/replay framework. Later
     checkpoints follow items 6 (`TYPEFAMILY`/`SUMTYPE` replay), 8 (generated
     constructor metadata), 14 (`ENUM`), 15 (`PRODUCT`), and 16 (`POLICY`) as
     they land; those are not prerequisites of this item's first checkpoint.
   - Goal mapping: keeps CLI/checker paths fail-closed and aligned with runtime.

6. **Implement `TYPEFAMILY` and `SUMTYPE` declaration grammar**
   - Paths: `src/core/checker.f`, `src/core/roles.f`, `src/habu/habu2.f`,
     `docs/type-families.md`.
   - Work: add package-aware public defining words for cell families and sum
     families. Each `VARIANT` block terminates with `;VARIANT` and each sum
     block with `;SUMTYPE`, matching the normalized spec grammar. Constructor
     and variant names are installed from lexed tokens and
     interned metadata, not interpolated source strings. Token grammar rejects
     delimiters, control words, qualified names in illegal positions, empty sums,
     uppercase or mixed-case family names, reserved signature/type tokens
     (`a`..`z`, builtins such as `n`/`f`/`r`, pointer/layout tokens such as
     `ptr`/`field`, atom prefixes, existing CT/VREC/type names), unknown payload
     types, and injection-shaped text. Qualified family references split the
     qualifier before case validation: uppercase package qualifiers are allowed
     (`PKG:result<n>`), but the family tail must be lowercase
     (`PKG:Result<n>` rejects). Top-level bad family declarations under
     multi-error mode must be reported and rolled back without requiring a fake
     declared stack signature.
   - Acceptance: fixtures define `TYPEFAMILY`, `SUMTYPE result`, `SUMTYPE option`,
     use them in signatures, and reject duplicate names, empty sums, bad params,
     uppercase/mixed-case family declarations or signature references
     (`TYPEFAMILY Result`, `SUMTYPE Result`, `Result<n>`), reserved family names
     (`TYPEFAMILY a 0`, `SUMTYPE n 0`, `TYPEFAMILY ptr 0`), unknown payload
     types, bad tokens, missing terminators, and package visibility violations.
     Qualified fixtures prove `PKG:result<n>` accepts and `PKG:Result<n>`
     rejects.
     `MULTI-ERR-BEGIN ... evaluate ... MULTI-ERR-END` tests prove bad family
     declarations do not poison later checks, and multi-error fixtures prove
     unknown-family and wrong-arity signatures report diagnostics while
     continuing to later definitions without storing invalid signature rows.
   - Risk: defining words must keep runtime and checker registries transactional
     on every failure path.
   - Effort: large.
   - Depends on: items 2-5.
   - Goal mapping: exposes the first ADT authoring surface without overloading the
     legacy enum/product surface yet.

7. **Add hidden physical fields and logical row expansion**
   - Paths: `src/core/checker.f`, `src/core/render.f`,
     `docs/type-families.md`.
   - Work: replace direct `SIG-TYPE MK-PUSH` with `PUSH-LOGICAL`. Cell families
     push one logical cell. Layout families expand to hidden physical field
     terms (`@family.slotN<...>`, `@family.tag<...>`), with the tag as the
     top-of-stack cell. Hidden field terms carry the same resolved `family-id`
     as the logical term; `@family...` names are diagnostic renderings only, not
     identity. Public signature parsing rejects `@...` names. This item also
     installs the hidden-field kind and fail-closed rejection in ordinary
     primitive binding; public layout-row expansion must not be enabled until
     item 12's width-aware lowering can preserve bundles.
   - Acceptance: constructor signatures render as logical types, internal rows
     contain hidden fields, public `@result.tag` signatures reject, and existing
     `field<...>` value-record behavior is either subsumed or preserved with
     explicit compatibility tests. Same-tail hidden fields from different
     packages cannot unify or compact as the same physical family. Before item
     12 lands, any checked path that would expose hidden layout rows rejects
     instead of allowing one-cell primitives to touch them.
   - Risk: diagnostics may leak hidden fields unless compaction lands with row
     expansion.
   - Effort: large.
   - Depends on: item 6.
   - Goal mapping: makes logical ADTs sound over Habu's physical cell stack.

8. **Generate constructors without emitted trust**
   - Paths: `src/core/checker.f`, `src/habu/habu2.f`, `docs/type-families.md`,
     `tools/trust-lint-core.f`, `TRUSTED.md`.
   - Work: generate checked constructor effects and runtime words from SUMV
     metadata. Constructors push payload cells, zero padding for absent variant
     slots, then tag. Generated ADT code must not emit `TRUST`, `TRUSTED:`, or
     `set-check`; do not add new `TRUSTED.md` rows for ADT constructors.
     Constructor wordlist identity is derived from `family-id` plus defining
     package id, not only from the uppercase family tail, and produces a legal
     single-colon caller spelling. The visible generated package name must be
     injective and stable across unrelated earlier declarations: it is derived
     by the pinned escape/hash algorithm in Package Shape (hyphen-escaped
     length-coded segments, SHA-256 fallback past the dictionary name limit),
     implemented byte-identically in native, `habu1`, and Gforth mirrors.
     It never uses allocation-order numeric package/family ids for visible
     spelling and never relies on raw hyphen concatenation alone, so `A-B` + `c`
     cannot alias `A` + `b-c`. Generated constructor bodies compile through
     existing checked literal/stack lowering plus item 12 width-aware paths —
     no new compiler keywords; native execution is proven here, and
     Gforth-recovered constructor parity is proven with item 10. Generated constructor package names are reserved and
     non-reopenable by ordinary `package`; if an ordinary package with the
     derived spelling or any qualified definition-created wordlist already
     exists, the family declaration rejects. Private families do not publish an
     external constructor package. This item records private constructor
     metadata only; item 9 introduces the source-level `construct family
     variant` token protocol. The metadata is keyed by family id and variant id,
     not by bare variant words, so private same-tail variants cannot collide or
     leak.
     This item remains metadata-only until item 12 lands: no public constructor
     package, private `construct` form, or runtime constructor body is enabled
     before native and Gforth width-aware lowering can preserve bundles.
     Until item 11 lands, constructors for layouts with linear or possibly
     linear payloads reject instead of publishing a public surface that could
     branch-drop or duplicate resources.
   - Acceptance: `RESULT:OK`, `RESULT:ERR`, `OPTION:NONE`, `OPTION:SOME`, and
     an arbitrary third sum family not named result/option/color
     type-check and run; wrong payloads reject; multi-cell payload variants
     prove `M > 1` max-payload width, zero padding, padding drop, and stack
     order for schemas such as `ptr u8 n`; raw tag constructors are not exposed;
     generated constructors publish only `OK`/`ERR` in package `RESULT`
     public and restore caller runtime+checker package state from global,
     unrelated package, and reopened package contexts; same-tail families in
     different packages publish disjoint, addressable constructor APIs and
     cannot collide on `RESULT:OK`; trust-lint, trusted-inventory,
     checked-boundary-lint, evaluated-source capture, and a generated-constructor
     audit prove no generated trust sites exist, including no `TRUST`,
     `TRUSTED:`, or `set-check` hidden inside strings later passed to
     `evaluate`. `checked-boundary-lint` must not treat `TRUSTED: ... set-check`
     as proof of no generated checker mutation, and `trust-lint` must scan code
     after backslash bytes inside string/path literals. No manifest rows are
     added. Fixtures prove an
     existing or later ordinary package or qualified definition with the
     injectively derived generated package name cannot hijack or collide with
     generated constructor packages. Fixtures prove private `SUMTYPE result`
     inside `package PKG` does not export any external generated constructor
     package. Fixtures prove
     two private families in one package may use the same variant tail without
     publishing bare `OK` or external generated constructor words; source-level
     private construction is not accepted until item 9 installs `construct`.
     `undefine RESULT:OK`, `undefine` of any generated constructor
     word, and `undefine` of generated package entries reject instead of
     deleting protected constructor metadata.
     Hyphenated package/family collision fixtures prove generated package names
     are injective (`A-B` + `c` and `A` + `b-c` cannot share a constructor
     namespace). Stability fixtures prove adding an unrelated package or family
     earlier in the source does not rename an existing generated constructor
     package.
     Linear-payload constructor fixtures reject until item 11 proves exact
     ownership accounting.
   - Risk: generated words must publish into the correct package/wordlist or
     they will revive duplicate/shadowing bugs.
   - Effort: large.
   - Depends on: items 7 and 12.
   - Goal mapping: provides ADT introduction forms without per-type trust rows.

9. **Add checker-owned `MATCH` token protocol and control semantics**
   - Paths: `src/core/checker.f`, `lib/task.f`, `docs/type-families.md`.
   - Work: before reserving `construct`, migrate the pre-existing task API
     words `CONSTRUCT` (`lib/task.f`) and their call
     sites to a non-colliding name; dictionary lookup is case-folded, so the
     lowercase reservation collides with the uppercase definitions.
     Then define a token protocol where private construction uses
     `construct family variant`, `MATCH` consumes the family token, and each
     branch consumes a variant token before `OF`; variants are not ordinary word
     lookups. `construct` consumes family and variant tokens while the owning
     package is open, resolves them to `(owning-package-id, family-id,
     variant-id)`, and records that tuple in checker/compiler capture. The
     checker must have constructor/match-mode token capture before normal
     dictionary lookup so branch names cannot collide with locals or words. Add
     growable or fail-closed `CF-MATCH` frames with family id, type args, base
     rows, seen variants, branch output rows, dead-path state, and source span.
     Overflow must reject with diagnostics, not silently mark a definition
     uncheckable. Public `MATCH` checking for layout values is not enabled until
     item 12 proves width facts reach native and Gforth lowering; before that,
     `MATCH` parser/capture metadata may exist only as reject-only scaffolding.
     Until item 11 lands, `MATCH` over layouts with linear or possibly linear
     payloads rejects; public matching for linear ADTs is enabled only with exact
     branch consumption/refinement proof.
     V1 has no default branch token; every variant must be explicit so
     exhaustiveness stays decidable and replay/reserved-token support stays
     narrow. A future default branch is a separate language extension with its
     own reserved token, diagnostics, and all-errors replay support.
   - Acceptance: private `construct family variant` resolves only inside the
     owning package and only through the checker-owned token protocol; bare
     variant words and generated external constructor words for private families
     do not resolve. Exhaustive matches certify; non-exhaustive matches,
     duplicate variants, wrong-family variants, missing family token, missing
     variant token, default-branch syntax, branch-output mismatches, and
     return-stack mismatches reject. Linear-payload match fixtures reject until
     item 11, then prove exact branch consumption once item 11 lands. Existing
     `CASE` fixtures continue to pass. Reserved-name lint proves no
     pre-existing `CONSTRUCT` definition remains once `construct` is reserved,
     and the migrated task API keeps its tests green.
   - Risk: `CASE` and `MATCH` share `OF`/`ENDOF` surface; parser dispatch must
     distinguish them without weakening either.
   - Effort: very large.
   - Depends on: items 7, 8, and 12.
   - Goal mapping: implements checked elimination, refinement, and exhaustiveness.

10. **Lower constructors and `MATCH` in native and bootstrap compilers**
   - Paths: `src/habu/habu1.f`, `src/habu/habu2.f`, `src/habu/aot-lib.f`,
     `src/habu/aot-closure.f`, `tools/hb-build-lib.f`, `lib/object.f`,
     `lib/object-cache.f`, `lib/object-index.f`, `lib/object-link.f`,
     `lib/object-resolve.f`, `tools/object-image.f`, `src/habu/macho.f`,
     `src/habu/elf.f`, `src/habu/driver-io.f`, `docs/census-tfam-10.md`,
     `bootstrap/cg/forth.fs`, `tools/compiler-dispatch-test.f`,
     `tools/bootstrap-codegen-test.f`, `docs/type-families.md`.
   - Work: add keyword data, label variables, label assignment, `EMIT-KWDATA`
     rows, and lowering for `MATCH`/`OF`/`ENDOF`/`;MATCH`, family/variant token
     consumption, constructor tag pushes, tag compare/branch chains, and
     invalid-tag die paths with no normal continuation in both native `habu2.f`
     and the Gforth bootstrap codegen. Add match-mode token capture before the
     normal local/keyword/literal/call/undefined path in both compilers. Add
     explicit object/AOT test-entry support for ADT bad-tag tests: the entry
     seeds raw physical payload slots and an invalid tag, then calls a generated
     checked `MATCH` helper instead of the normal zero-argument `MAIN` path.
     AOT closure roots, export records, object-image selected-entry handling,
     object schema/index/cache metadata, artifact-cache metadata, and
     content-addressed cache keys must include the selected dictionary identity
     (package/WID/record id, not bare name), helper root identity, seeded stack
     cells, layout/test mode, and ABI/source digest. A normal `MAIN` object and
     a preseeded bad-tag object are different artifacts, and object-image can
     start at a selected nonzero/non-`MAIN` entry.
   - Acceptance: valid matches execute all branches, including an arbitrary
     third sum family not named result/option/color. An invalid-tag fixture dies
     deterministically at runtime on both a native self-hosted candidate and a
     no-binary Gforth-recovered candidate. Checked test-only object/AOT entry
     support seeds the raw payload/tag cells and calls the generated helper; the
     support may use existing image-writer trust rows but must not add ADT
     `TRUST`, `TRUSTED:`, `set-check`, or manifest rows. Codegen
     no-continuation sequence assertions are additional
     evidence, not a substitute for runtime bad-tag execution. Bad-tag fixtures
     cover a one-payload sum, a wider max-payload sum, and a zero-payload enum or
     sum, including at least one arbitrary family not named result/option/color,
     so fallback cleanup proves every payload slot plus tag is handled. AOT
     closure tests prove the helper is not stripped, no `aot: no MAIN` fallback
     occurs for non-`MAIN` test entries, and stale normal-`MAIN` objects cannot
     satisfy preseeded bad-tag runs because entry/preseed metadata is present in
     the artifact cache, object schema, object index, object cache, and restore
     keys. Tests cover same-name entries in different packages/WIDs, nonzero
     object entry offsets, and helper-root identity so bare string lookup cannot
     pick the wrong `MAIN` or helper.
     Native fixpoint is byte-identical; no-binary Gforth bootstrap reaches fixpoint;
     compiler-dispatch and bootstrap-codegen tests cover the new keywords and
     prove existing `CASE` shape unchanged.
   - Risk: virtual stack register state must treat physical bundles atomically
     across branch prologues and epilogues.
   - Effort: very large.
   - Depends on: items 9 and 12.
   - Goal mapping: makes ADTs executable and fail-closed at runtime and recovery.

11. **Add linear/resource semantics for layout values**
    - Paths: `src/core/checker.f`, `test/engine-suite.f`,
      `docs/type-families.md`.
    - Work: add `LAYOUT-LINEAR?` and `LAYOUT-LINEAR-COUNT` over expanded layout
      fields so any layout value containing a linear payload is linear. Reject
      raw `drop`, copying, branch loss, and unconsumed linear ADTs just as for
      scalar linear values. Extend effect-taint / polymorphic-copy laundering
      checks from scalar `LIN-CON?` to layout values.
   - Acceptance: fixtures prove `result<own,n>` cannot be dropped or copied,
     `MATCH` consumes/refines linear payloads exactly once, polymorphic
     laundering through `[: dup ;] execute`, `KEEP`, `BI`, deferred calls, or
     delayed type resolution rejects, and non-linear ADTs retain scalar behavior.
   - Risk: layout expansion and linear counting must agree or linear payloads can
     be laundered through hidden fields.
   - Effort: large.
   - Depends on: items 7-10 and 12.
   - Goal mapping: preserves checker soundness for resource-bearing ADTs.

12. **Make all checked stack operations layout-aware**
   - Paths: `src/core/checker.f`, `src/core/combinators.f`, `src/habu/jit.f`,
      `src/habu/habu2.f`, `src/habu/habu1.f`, `src/habu/regalloc.f`,
      `bootstrap/cg/forth.fs`, `bootstrap/cg/jit.fs`, `docs/type-families.md`.
    - Work: track logical widths and make every stack primitive that can touch
      layout bundles operate on logical values: `dup`, `drop`, `swap`, `over`,
      `nip`, `rot`, `-rot`, `tuck`, `2dup`, `2drop`, `2swap`, `2over`,
      plus both optimized JIT shuffles and fallback spilled calls. Add a
      compiler data path for width-aware lowering: either check/resolve the body
      token stream before emission or feed per-token checker width/refinement
      facts into native and Gforth emitters before any one-cell `VSHUF` code is
      emitted. This includes optimized one-cell arithmetic/comparison/float
      lowering (`VOP*`, `VCMP`, `VUN`, `FOP`) and bootstrap dispatch, not only
      shuffles. Checker-after-emission is not sufficient. Return-stack transfers
      (`>r`, `r>`, `r@`, `2>r`, `2r>`, `2r@`) and locals bind/ref must be
      width-aware or explicitly reject layout bundles. Native and bootstrap raw
      primitive bodies must either become bundle-aware or reject checked layout
      bundles before falling back to one-cell primitives. Reject layout types in
      cell-only family parameters until layout-polymorphic params are supported.
      Hidden physical fields are a checker-owned kind: ordinary `a`/`ptr a`
      polymorphic primitives (`0=`, `@`, `!`, atomics, comparisons, etc.),
      concrete primitive effects (`PE-N`, `PE-PTR-*`, comparisons, arithmetic,
      unary ops), optimized one-cell lowering paths, and final signature
      coercions such as `FIELD-COERCE?` must reject them unless the operation is
      an approved checker-owned layout primitive, constructor, or `MATCH`
      lowering step. User signatures, quotation application (`execute`),
      `catch`, defer calls, and combinators (`DIP`, `KEEP`, `BI`, etc.) must not
      bind hidden fields to ordinary type variables or split bundles through
      higher-order effect application. Scalar control predicates and loop/control consumers
      (`if`, `while`, `until`, `case`, `of`, `do`, `?do`, `+loop`, and related
      native/Gforth branch-lowering paths) must reject hidden fields before
      one-cell predicate or loop-bound codegen runs. Top-level defining words that consume stack values
      (`constant` first, then any future
      value-consuming definer) must either define multi-cell layout constants
      soundly or reject layout values before native and bootstrap one-cell pop
      paths run. Stack introspection words such as `depth` and `.s` must report
      logical stack shape or reject rows containing hidden fields; they must not
      expose raw physical cell count or hidden field names.
      Interpret/top-level execution is part of the surface: either it maintains
      runtime logical-stack metadata so native and Gforth interpreter primitives
      preserve/reject whole bundles, or it rejects layout values before public
      constructors can leave them on the interactive stack. A compiled-only fix
      is insufficient. Nested `evaluate`/recovery frames, `catch`/`throw`
      nonlocal exits, and `run-in-stack` fresh-stack execution must save and restore
      data-stack and return-stack logical metadata, including return-stack depth
      and hidden-field layout tags, or reject layout bundles before frame
      entry/exit so evaluated source cannot split or strand hidden fields.
      `?dup` is not a generic bundle copy. It has no checker axiom today — it
      exists only as emitter primitive `BQDUP` (`src/habu/habu1.f`,
      `bootstrap/cg/forth.fs`) branching on the raw TOS cell — so this item
      first adds its axiom, then makes it reject layout ADTs unless the
      family declares a checked truthiness/niche policy that specifies which
      physical representation is false and proves the copied bundle is valid.
      Tag 0 is a valid variant for common sums such as `option` and `result`, so
      raw top-cell truth is unsound.
      Until item 11 lands, any layout bundle containing a linear payload or an
      unresolved payload that may become linear rejects copy/drop operations
      rather than treating it as a freely copyable layout value.
   - Acceptance: fixtures prove every listed primitive preserves whole bundles;
     primitive-effect axiom census is updated; scalar behavior is unchanged;
     layout-in-cell-only positions reject with a clear diagnostic; return-stack,
     locals, interpret-mode stack primitives, `?dup` rejection, `constant`,
     `depth`, `.s`, fallback primitive, and bootstrap recovery fixtures prove
     layout bundles cannot be split or leak on the exact native and Gforth paths
     above. Gforth scalar inventory parity for `2>r`, `2r>`, and `2r@`
     (confirmed absent from `EMIT-STACK-PRIMS`,
     `bootstrap/cg/forth.fs`) is either
     implemented or explicitly rejected fail-closed before layout tests run.
     Codegen fixtures prove width facts reach
     lowering before emission, and negative fixtures prove hidden fields cannot
     bind to any ordinary primitive effect, optimized lowering path, or
     scalar control predicate, loop/control consumer, or field-coercion path
     outside constructors/`MATCH`; higher-order effect fixtures prove user
     signatures, quotations, `execute`, `catch`, deferred calls, and
     combinators reject hidden-field binding. Linear-containing layout
     fixtures reject copy/drop until item 11's linear accounting proves exact
     consumption. Fixtures cover the full primitive axiom table, `VOP*`, `VCMP`,
     `VUN`, `FOP`, bootstrap dispatch, nested `evaluate`, `catch`/`throw`, and
     `run-in-stack` frames in native and Gforth-recovered candidates, including
     throws across `>r`/`r>` with hidden fields on the return stack.
   - Risk: primitive effects currently describe one-cell stack operations; this
     item changes checker semantics and native lowering together.
   - Effort: very large.
   - Depends on: item 7.
   - Goal mapping: prevents hidden physical layout from leaking into user code.

13. **Compact diagnostics, repair packets, and public signatures**
    - Paths: `src/core/render.f`, `docs/repair-diagnostics.md`,
      `tools/repair-schema-doc-test.f`, `tools/repair-packet-core.f`,
      `tools/gate-json-assert-core.f`, `tools/public-signatures-core.f`,
      `tools/trusted-inventory.f`, `docs/type-families.md`.
    - Work: make row collection bounded/growable, then render hidden-field runs
      as registered lowercase logical
      `family<args>` values, extend SGBAD/diagnostic state for expected/got
      arity, family, variant, and payload data, and keep repair packets stable
      for LLM consumers. Extend the repair diagnostics schema with explicit
      machine-readable ADT fields/classes for family id/name, arity,
      variant/tag, payload position, expected type, and actual type; do not
      collapse ADT failures into only generic expected/actual strings. Logical
      rendering includes package identity in JSON fields and disambiguates
      human text when same-tail families are in scope. Public signatures for
      generated constructors are synthesized from TFAM/SUMV metadata. Repair
      packet construction and JSON packet assertions must preserve the new ADT
      fields end-to-end for LLM repair consumers. Add a non-definition
      diagnostic shape for top-level declaration errors (`TYPEFAMILY`, `SUMTYPE`,
      `VARIANT`, etc.) that does not require fake `word`, `declared_effect`, or
      `inferred_effect`, `definition_source`, `source_excerpt`, `return_stack`,
      `expected`, or `actual` fields. Declaration packets carry declaration
      kind, family id/name when available, variant/tag when available, arity,
      package id/name, source span, and error class. Extend gate JSON assertions
      and repair packets to accept that shape explicitly instead of routing it
      through definition-only assertions. Public-signature tokenization must
      follow executable Forth's standalone-comment rule, not treat every
      paren-prefixed word as a comment; words such as `(CMP)` remain visible
      when they carry public signatures. Every new ADT repair class has a
      stable `GJA-SUGGEST-FOR` mapping.
   - Acceptance: oversized physical rows reject or grow deterministically before
     rendering; negative tests show lowercase logical `result<ptr u8,n>`
     diagnostics, not `@result.slot0`; same-tail package families remain
     distinguishable in JSON and public signatures; wrong family/arity/variant
     errors include the ADT fields above; repair schema docs and public
     packet fixtures include logical ADT constructors and preserve ADT fields;
     declaration-error fixtures prove no fake signatures or definition-only
     stack fields are required; every new ADT repair class has a gate assertion
     suggestion; `(CMP)`/paren-word fixtures prove public-signature lexer parity
     with executable Forth and trusted-inventory tokenization; public signatures
     are synthesized from metadata and never expose hidden fields.
   - Risk: compaction must be deterministic and not hide genuine low-level field
     mismatches in checker-internal diagnostics.
   - Effort: medium.
   - Depends on: items 5, 7, 8, 9, and 12.
   - Goal mapping: preserves LLM-facing error quality.

14. **Implement enum families and migrate legacy enums**
    - Paths: `src/core/enums.f`, `test/gate-dictionary-lib.f`,
      `docs/type-families.md`.
    - Work: replace or retire the current numeric `ENUM`/`ENUM4` chain before
      publishing block-style enum families. If compatibility is needed, move the
      old surface behind an explicit legacy name and update all call sites.
   - Acceptance: existing enum fixtures either pass through the deliberate
     legacy spelling or are migrated; block-style `ENUM color ... ;ENUM`
     defines checked constructors and exhaustive `MATCH`; duplicate/missing/bad
     enum variants reject.
   - Risk: reusing `ENUM` without a transition will break existing dictionary
     tests at load time.
   - Effort: medium.
   - Depends on: items 9-13.
   - Goal mapping: implements enum families without silently changing legacy
     semantics.

15. **Implement product families and migrate value records**
    - Paths: `src/core/structures.f`, `src/core/roles.f`, `lib/ptx/ir.f`,
      `test/engine-suite.f`, `lib/object.f`, `lib/object-test.f`,
      `src/habu/aot-lib.f`, `docs/effects.md`, `docs/type-families.md`.
    - Work: before reserving `FIELD`, migrate the pre-existing `FIELD` words
      (`lib/object.f`, `lib/object-test.f`, `src/habu/aot-lib.f`)
      and their call sites to non-colliding names; dictionary lookup is
      case-folded. Then implement `PRODUCT ... ;PRODUCT` after layout-aware
      stack operations are proven. Decide by evidence whether `VALUE-RECORD` becomes
      product-family sugar or remains a typed compatibility layer over the same
      registry. Migrate production users such as PTX IR only after fixtures prove
      by-value construction/destructure and no size regression.
   - Acceptance: existing value-record fixtures pass; product fixtures cover
     by-value construction/destructure, hidden fields, logical rendering, package
     visibility, and linear payloads; reserved-name lint proves no pre-existing
     `FIELD` definition remains once `FIELD` is reserved, and the renamed
     object/aot helpers keep their tests green; docs distinguish supported and
     legacy surfaces.
   - Risk: immediate migration can churn PTX IR; compatibility is acceptable only
     if it is typed, tested, and registry-backed.
   - Effort: large.
   - Depends on: item 12.
   - Goal mapping: folds existing by-value records into the generic family design.

16. **Implement layout policies**
    - Paths: `docs/type-families.md`, `src/core/checker.f`.
    - Work: parse and validate `POLICY`. Ship `stack-cell-tag` as the required
      default and implement explicit diagnostics for invalid or unsupported
      policies. Then add packed-tag, niche-null, and boxed policies as separate
      checked extensions with layout tests before exposing them publicly.
   - Acceptance: missing policy defaults to `stack-cell-tag`; invalid policies
     reject; recursive or unsupported layouts reject with the documented
     diagnostic; each implemented policy has constructor, match, stack-op, and
     invalid-tag tests.
   - Risk: packed/niche/boxed policies change physical layout and can break
     compiler assumptions if exposed before lowering support.
   - Effort: large.
   - Depends on: items 9-15.
   - Goal mapping: completes the design beyond the default unboxed stack layout.
   - Follow-on consumer capability (dotted, outside this campaign's items):
     checked buffer store/load for layout-family values plus a typed
     array-of-ADT container over the packed ABI descriptor —
     `habu-checker-capability-typed-a480c423`. First consumers are maki
     Model CAD report tables, schedule measurement history, and
     artifact-cache rows (`docs/model-cad.md`, fable branch). Boxed policy
     remains the maki recursive-IR unlock; packed-tag is this dot's
     prerequisite.

17. **Gate, bootstrap, size, cache, and trust proof**
    - Paths: `test/engine-suite.f`, `test/gate-dictionary-lib.f`,
      `tools/check-test-lib.f`, `tools/trust-lint-test.f`,
      `tools/trusted-inventory.f`, `tools/checked-boundary-lint-core.f`,
      `tools/filemap-lint.f`, `tools/host-lint.f`, `test/gate-build-size.f`,
      `test/run-files.f`, `tools/srclist.f`, `tools/hb-build-lib.f`,
      `test/run-result-cache-test.f`, `tools/build-fixpoint.f`,
      `tools/bootstrap.sh`, `src/habu/aot-capture.f`, `src/core/roles.f`,
      `lib/ffi.f`, `tools/trust-lint-core.f`, `FILEMAP.md`, `docs/bootstrap.md`,
      `bootstrap/cg/*.fs`, `bootstrap/src/*.fs`, `bootstrap/*.fs`, `TRUSTED.md`.
    - Work: this is not a last-only cleanup phase. For every item 2-16, add TDD
      fixtures before implementation, rebuild `bin/hb`,
      prove native self-refresh/fixpoint, run focused checker and engine suites,
      update build-cache ABI/source keys, update `test/run-files.f` result-cache
      keys, result-cache closure lint, and `tools/srclist.f` canonical source
      order for any new core files. The existing stdin driver closure must be
      reconciled too through one exact stdin manifest shared by
      `tools/build-fixpoint.f`, `tools/bootstrap.sh`, `tools/srclist.f`,
      `tools/hb-build-lib.f`, and `test/run-files.f`: `src/core/include.f`,
      `src/habu/aot-capture.f`, and `src/habu/stdin.f` must either all be in
      the keyed closure where they are loaded or be explicitly proven outside it.
     Result-cache closure lint must cover `TR-UNDER-SOURCE-FILES` and every
     under-source/native candidate set, not only debug/AOT-negative examples.
     Repair trust proof tooling before relying on it: generated/evaluated
     source passed to `evaluate` must be captured or statically forbidden for
     ADT generators; `checked-boundary-lint` must have a mode that treats
     `TRUSTED: ... set-check` as generated checker mutation when proving ADT
     code; `trust-lint` must not discard code after backslash bytes inside
     string/path literals and must share the trusted-inventory full-file lexer so
     executable tokens between literal strings, split-line trust shapes, and
     dynamic trust targets are classified as bare unless the manifest pins the
     actual executed target/effect. Trust ratchets compare the exact trust manifest and
     inventory counts before and after each item. The type-family/ADT campaign
     may not add `TRUST`, `TRUSTED:`, `set-check`, or `TRUSTED.md` rows unless a
     separate non-ADT dot is approved and proven outside this plan. Run
     trust/filemap/host/public-signature lints, and record candidate binary size
     before/after every large item.
   - Acceptance: full native suite passes on macOS; Linux/zed proof passes for
     any OS/runtime emission changes; no-binary Gforth bootstrap succeeds; the
     Habu-under-test candidate passes `GE-CANDIDATE-SIZE-CHECK` against
     `test/gate-build-size.f`, and any baseline change is justified by byte-map
     RCA in the same commit; `tools/filemap-lint.f` covers
     `docs/type-families.md`; result-cache closure tests cover new
     TFAM/MATCH/source files plus the ordered event surface for `include`,
     `included`, `require`, `required`, `provided`, generated/escaped paths,
     stack-string path materialization, and the current `aot-capture.f` stdin
      source closure across every stdin builder/cache/list entry. Trust proof
     fixtures include evaluated-string trust attempts, `TRUSTED: ... set-check`,
     backslash-before-trust lines, intervening-token trust shapes such as
     `s" FOO" (CMP) s" n -- n" TRUST`, split-line trust, and dynamic trust
     targets so generated ADT trust cannot hide from inventory.
     Generated/evaluated source artifacts, object/AOT emitted source,
     and materialized temporary loaders are scanned, not only raw source files;
     fixtures with `S\" ... TRUST ...\" evaluate`, comment/backslash placement,
     escaped strings, and generated source fail the ratchet. Checked-boundary
     source-list tests prove original input files and discovered dependencies
     are scanned, not only the materialized loader. Filemap-lint covers active
     bootstrap recovery sources under `bootstrap/`, including the Gforth mirrors
     required by items 10 and 12. Existing trusted compiler/image boundaries may
     be tightened or refactored with audited effect/manifest updates, but ADT
     implementation does not add new trust rows; if a new trusted boundary is
     required, it is a separate prerequisite dot and this plan blocks until it
     lands. Master advances only after
     exact-tree green proof.
   - Risk: checker/compiler staging can break recovery bootstrap or cache
     invalidation even when native self-refresh works.
   - Effort: large.
   - Depends on: every code item.
   - Goal mapping: keeps master green and prevents trust, cache, bootstrap, or
     size regressions.

## Dependency Order

1 -> 17a -> 2 -> 17b -> 3 -> 17c -> 4 -> 17d -> 5 -> 17e -> 6 -> 17f -> 7 -> 17g -> 12 -> 17h -> 8 -> 17i -> 9 -> 17j -> 10 -> 17k -> 11 -> 17l -> 13 -> 17m -> 14 -> 17n -> 15 -> 17o -> 16 -> 17p.

Item 7 installs hidden-field metadata in reject-only form. User-callable
constructors, public layout rows, and `MATCH` lowering wait until item 12 can
preserve bundles before native/Gforth emission. Item 17 is the per-item proof
gate, not a final phase: no implementation item can land without its red tests,
source-list/filemap/cache updates, focused checks, size/trust proof, and
bootstrap/fixpoint proof where applicable. Item 13 runs continuously once item 7
exists, but cannot finish until layout-aware stack operations, enum/product
migration, and policies are proven.

## Review Log

- Round 1 baseline: `nwzwknmu` / `28ac22f97cde`, PLAN
  `9e951f67cca276a1ab909b672dbce808c2cf3b76c313dc78e8e83baa789e7a02`, spec
  `14e2477bb8bd4d92c14ddf3d506eddf1a3dcfeadf61cb403de761f3bdbbc51f6`.
- Agents: plan-critic, edge-case-hunter, reviewer, scout, code-auditor,
  destructor.
- Accepted Critical/Major findings folded into this draft: source preverify,
  rollback, package scoping, safe generated token grammar, generated-trust
  prohibition, nested param parsing, `MATCH` token protocol, CF frame
  fail-closed behavior, bootstrap compiler mirror, linear layout accounting,
  full stack-primitive coverage, primitive axiom census, enum/product staging,
  layout policies, richer diagnostics, public signatures, trust inventory,
  checked-boundary lint, hb-build cache keys, and missing negative cases.
- Round 2 accepted findings folded in (the `TFAM:*` point is superseded by
  Round 8: mutators are sealed friend-only, not public `TFAM:*` words):
  `TYPE` uses public `TFAM:*` APIs instead
  of private arena helpers; generated constructors publish into the family
  package public wordlist and restore caller package state; all-errors replay
  preserves family support declarations; rollback frames are reentrant; `MATCH`
  captures family/variant tokens before normal lookup; return-stack, locals,
  bootstrap fallback primitives, and row growth are explicit work items.
- Round 3 accepted findings folded in: registered type terms store resolved
  `family-id`; rollback saves package and DFER state; staged rollback acceptance
  does not depend on future definers; all-errors replay preserves existing
  source-local declarations; multi-error invalid-family signatures continue
  without storing invalid signatures; stack coverage includes `?dup`, exact
  local/return-stack native paths, and Gforth mirror paths. Type names in
  signatures stay lowercase system vocabulary (`result<...>`), while callable
  constructor words remain package-qualified project words (`RESULT:OK`).
- Round 4 accepted findings folded in: all replay/copy paths preserve
  `family-id`; hidden physical field identity is id-based, not name-shaped;
  uppercase/mixed-case type-family declarations and signature references reject;
  all-errors source-list redrive covers original entries and bad family
  declarations; same-tail constructor packages cannot collide; value-consuming
  `constant` and introspection words `depth`/`.s` cannot split or leak layout
  bundles.
- Round 5 accepted findings folded in: package-local constructors have legal
  one-colon spellings; bare zero-arity families store `family-id`; same-tail
  diagnostics/public signatures retain package identity; growable schema args
  remove the fixed `PARAM-MAX-ARGS` cap; public signatures and repair packets are
  registry/schema-driven; filemap, size ratchet, source-list, and result-cache
  proof paths are explicit; generated constructors add no TRUST rows; width facts
  reach native/Gforth lowering before emission; hidden fields cannot bind to
  ordinary polymorphic primitives.
- Round 6 accepted findings folded in: the normative spec now forbids trusted
  generated constructors; `docs/type-families.md` is a required filemap-lint doc;
  all-errors replay includes `require`/`required` dependency closure; repair
  packet builders preserve ADT fields; family names reject reserved type tokens;
  generated constructor package names are reserved/non-reopenable; qualified
  family references split qualifier case from lowercase tail validation; hidden
  fields reject every ordinary primitive kind, optimized lowering path, and
  `FIELD-COERCE?`; linear layout semantics now depends on layout-aware stack
  operations.
- Round 7 accepted findings folded in: stale `bin/hb` with baked
  `src/core/result.f` is a blocking acceptance failure; private family
  constructors do not export external constructor packages; generated
  constructor package names are protected from qualified-definition hijack;
  `TFAM`/`TYPE`/`MATCH` are sealed system packages; top-level declaration errors
  have a non-definition diagnostic shape; layout constants are covered in
  verify-source, all-errors, and public-signature paths; ADT repair classes get
  gate assertion suggestions; item 7 cannot expose public layout rows before
  primitive isolation/width lowering; item 12 includes Gforth optimized shuffle
  paths and rejects possibly-linear layout copies until item 11.
- Round 8 accepted findings folded in: system registry mutators are not
  user-callable `TFAM:*` exports; `undefine` must reject sealed system and
  generated constructor entries; private constructors use a family-qualified
  checker token protocol instead of dictionary words; source replay covers
  `include`/`included` as well as `require`/`required`; hidden layout remains
  reject-only until width-aware lowering; `?dup` rejects layout ADTs without a
  declared truthiness/niche policy; stack-op coverage now names every item 12
  native/Gforth surface; invalid-tag tests do not introduce new trusted helpers.
  The stale-binary finding from this round was rejected as stale after the
  no-binary bootstrap rebuilt `bin/hb` and `filemap-lint` passed on the rebuilt
  image.
- Round 9 accepted findings folded in: sealed/generated WIDs are protected
  against raw `set-current`, exposed WID, checker package mutator, lifecycle
  truncation, and arbitrary-tail publish paths; source replay is an ordered
  event log that preserves include multiplicity, require/provided exact-string
  state, and package/checker deltas; public-signature extraction and repair
  schema generation share that event log; declaration repair packets no longer
  require definition-only fields; public constructors and `MATCH` explicitly
  depend on width-aware native/Gforth lowering; interpret/top-level mode must
  preserve or reject layout bundles; private constructors have concrete
  `construct family variant` syntax and compiler/checker capture; invalid-tag
  runtime proof uses object/AOT test-entry support without new ADT trust and is
  proved by execution on both native and Gforth-recovered paths; `construct` is
  reserved and replayed; v1 removes default `MATCH` branches; replayed path
  strings are byte-escaped or rejected; scalar control predicates reject hidden
  fields before one-cell native/Gforth lowering.
- Round 10 accepted findings folded in: protected WID coverage names the actual
  `XREF-START`/`XREF-LEN`/`XREF`/`LATEST` handle paths and raw dictionary memory
  writes, not only `XREF-WORDLIST`; protection is case-insensitive and applies
  to native, `habu1`, and Gforth bootstrap mirrors; source-composition events
  are collected by runtime include/require/provided instrumentation rather than
  static scanning alone; replay covers `S\"` loader forms or rejects unsupported
  string openers, preserves existing `constant`, `create`, `variable`,
  `TRUSTED:`, `TRUST`, `undefine`, and `EXPORT` support forms, and keeps
  reserved-name/checked-boundary lints on original source-list files; path
  escaping owns `lib/source.f` materialization; result-cache closure covers the
  full event surface plus `aot-capture.f`; constructor package spelling is
  injective for hyphenated package/family names; invalid-tag proof requires
  runtime execution on both native and Gforth-recovered candidates, with
  object/AOT test-entry support added if needed; item 12 covers the full
  primitive axiom table, `VOP*`/`VCMP`/`FOP`, bootstrap dispatch, and nested
  `evaluate` frame metadata; generic third-sum fixtures prevent
  result/option-only implementation.
- Round 11 accepted findings folded in: sealed/friend coverage now owns
  `immediate`, raw code-pointer exposure, direct legacy checker registry
  mutators, read/execute lookup paths, every raw memory write family, native
  `habu1`, and Gforth mirrors; replay uses a pre-lint restricted discovery pass,
  event-closure lints, DIAG-FILE escaping, support-only declaration diagnostics,
  and one exact stdin source manifest; generated/evaluated trust escapes,
  `TRUSTED: ... set-check`, and backslash trust-lint holes are explicit proof
  targets; item 12 covers higher-order effects, `VUN`, `catch`/`throw`, and
  `run-in-stack`; `SC-QUOT`, multi-cell payload padding, generic invalid-tag
  fixtures, legacy `ENUM` reservation order, and linear ADT public-surface
  gating are explicit.
- Round 11 follow-up findings folded in: Gforth `MATCH` support includes keyword
  data, label variables, label assignment, and `EMIT-KWDATA`; bad-tag proof
  requires explicit object/AOT preseed entry support and covers one-payload,
  wide-payload, and zero-payload layouts; source replay has a restricted
  discovery pass before preverify/static consumers plus current-token span
  capture; item 5 is staged to avoid depending on ADT grammar before item 6;
  `ENUM` is not reserved before the legacy enum surface is migrated; protected
  WID coverage includes `get-current`, `search-wl`, checker metadata mutators,
  `immediate`, lower XREF mutators, byte stores, and atomics.
- Round 12 accepted findings folded in: protected namespaces cover
  `postpone`/`compile,`/xt execution sinks, raw syscall and FFI writer
  boundaries, protected pointer provenance, AOT protected-WID restore with
  `WIDN` advancement, stable generated constructor package spelling that does
  not use allocation-order ids, item 8 as private-constructor metadata only,
  item 9 as the `construct family variant` source protocol, AOT closure and
  object-cache metadata for non-`MAIN` bad-tag entries, exact no-new-trust
  ratchets, and generated/evaluated artifact scanning for hidden trust.
- Round 13 accepted findings folded in: generated constructor packages are
  closed but public constructors remain callable/compilable; writable-buffer
  protection covers the complete primitive/syscall census, not only examples;
  AOT seed WIDs must not truncate to u8; object/AOT entry metadata reaches
  artifact cache, object schema/index/cache/restore, selected-entry image
  writing, and package/WID/record identity; sealed system packages get an
  internal source-origin friend reopen path; existing trusted compiler/image
  boundaries may be audited without adding ADT trust rows; filemap-lint covers
  `bootstrap/`; checked-boundary source-list lint scans original inputs; and
  public-signature lexing has paren-word parity fixtures.
- Round 12 follow-up findings folded in: AOT seed record registration and
  bootrun reject sealed/generated WIDs; `snap-rebase` is friend-only or
  protected-range checked; raw FFI/syscall writers, tick/postpone/compile paths,
  `hb-build` replay-closure cache keys, fresh require/provided discovery state,
  loader-word mutation fail-closed behavior, return-stack metadata across
  `catch`/`throw`/`evaluate`, trust-lint lexer parity, legacy `ENUM` phase
  split, `SC-QUOT` line ownership, per-item TDD/proof gates, package-owned
  source-file splits, and generic invalid-tag proof beyond result/option/color
  are explicit.
- Round 14 accepted findings folded in: `construct` and `FIELD` reservations
  first migrate the pre-existing `lib/task.f` `CONSTRUCT` and
  `lib/object.f`/`lib/object-test.f`/`src/habu/aot-lib.f` `FIELD` words; the
  source-origin friend capability is a boot latch set during the engine's
  canonical `tools/srclist.f` load and sealed before any user source runs, with
  no user-supplied source-list file friend-origin; the generated constructor
  package derivation is pinned to one hyphen-escape/SHA-256 encoding shared by
  native, `habu1`, Gforth, and the spec; item 5's first checkpoint depends only
  on items 2-4, removing the formal 5<->6 cycle; item 8 constructor bodies
  lower through existing checked paths with Gforth parity proven at item 10;
  `;VARIANT` joins the definer inventory, item 6 grammar, and normalized
  spec examples; item 13 declares its item 8 dependency; the stale Round 2
  `TFAM:*` log entry is marked superseded.

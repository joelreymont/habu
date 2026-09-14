# Finish Habu's native compiler

Status: implementation in progress, 2026-09-14. Cedar owns integration. This replaces
the obsolete IR/GPU migration plan, preserved in jj history. Reuse the existing
Tender campaign `habu-compile-the-tender-8385810f`.

## Current integration and review pins

`cedar/compiler-integration` is the reviewed integration branch. The external
review pins remain frozen: `review/current-compiler` at `51546316`, and
`review/partial-payload` at `f091a068`. The earlier August 23 `master` review
(`3c6bda9d`) predates these compiler changes. None is an accepted replacement
engine; source review and focused controls are not a rebuilt runtime gate.

The integration includes complete address rows and layout translation, native
publication provenance, typed `is`, indexed dictionary append, CAST identity,
catch grouping, portable partial-effect graphs, repeated registry persistence,
frozen IR reader reuse and canonical engine aliases through symlinked roots.
No accepted replacement engine is published.

The latest complete native gate is M (`99caf411`): all 343 suites ran in
429.559 seconds, with 336 passing and seven failing. The failures were the two
clobber lint entries, stage0 atomic guard inventory, WID/wide/chain capture,
and legacy build-fixpoint fixtures. The three lint entries are now repaired,
independently reviewed, and pass focused real-source checks on M. The current
native writer also passes the complete wide-artifact suite (BIG, EXT, XTLIT and
PREWIN), rc0 in 171.233 seconds. The complete gate has not been rerun with these
corrections. Wide-suite evidence: `/tmp/cedar-current-fixture-gate/`.
Exact gate results: `/tmp/cedar-M-full-suite.{json,log}`.

Current candidate M is source `99caf411`, SHA-256
`9522a8e5685129b17b107bd547dc0797a1f11e0bb3f89f8282b2b8206770b58c`.
It built through the optimizing native entry on K2 in 135.602 seconds. All eight
focused suites pass: real registry persistence at both tiers, three-generation
application capture, concurrent address registration and restore, worker
lifecycle, checker-owner descriptors, source-authority recovery and the complete
engine suite. The application fixture preserves identical engine-prefix,
region and DATA extents in all three generations, then restores checked
application execution. Logs and commands: `/tmp/cedar-M-focused/`.
This candidate is not yet an accepted replacement for downstream pins.

The composition includes growing address storage with actual aggregate artifact
admission, shared image ownership for worker quotation stores, synchronized
first registration and process mutex reset. Scoped diagnostic recovery retains
no executable authority. The cold prefix now declares its public owner ABI
constants and publishes its first complete checker owner; replacement checkers
still require explicit transfer. These changes have independent source review
and focused acceptance. General warmed-verifier source-order behavior remains
open in `0c9fe3d7`.

The WID/chain fixtures are moving to the current native writer with partial
captures that preserve their actual cold-prefix behavior. Both real sealed and
open WID collision controls pass. The complete current-writer WID run previously ended at
the unassigned-defer capture refusal (`4ccf56d9`), before its BIG/EXT/PREWIN
tail. The cold owner guard dependency is repaired and independently accepted
(`023e338e`, integrated as `d5ddce87`): the genuine empty native image keeps
VALIDATE below the core mark and passes prefix/descriptor controls; unchanged M
fails the new prefix assertion. The real cold compiler capture now reaches six
declared callback cells targeting exact global prefix entries. The named CODE
address repair is now independently reviewed and integrated as `77ec3869`;
named code-literal identity is integrated as `41876234`. Both pass focused
capture/read/fresh-seed controls, including exact target identity and private,
hidden, post-cut and nonentry refusals. Artifact version 9 carries named CODE
rows through the existing name pool. Complete WID/chain and native gate
acceptance remain pending the combined N build from `41876234`.

The registrar index and scoped unlock correction are integrated as `ec91419b`
and `0af37f52`, after independent review and focused growth, OOM, concurrency,
rewind, caught-refusal and snapshot tests. A registrar-only pair reduces
32,768 registrations from 966.486 ms to 0.896 ms; this is not a whole-build
measurement. The immutable TASK entry is integrated as `a4d2cc3d`, with
independent review, checked field-layout validation and three-generation
concurrent UDP/serial image acceptance. Its native candidate passes the
unchanged host-I/O test in 8.475 s. These fixes join the single combined N gate.

Tender's newer rules142 workload exposes a separate native CASE lowering
underflow (`a9d7bef5`), reduced to a default arm consuming preceding stack
values. The JIT control passes; native lowering incorrectly compares the
default's final depth with the CASE entry depth. Its repair and both-tier
regressions are in progress in `cedar-native-case`; no application workaround
or JIT fallback is accepted.

The reviewed dynamic effect pool is integrated at `8f0749b3`. Its actual-source
checks preserve 1,536 effects / 847,893 bytes through file and owned transfers,
including merge and malformed-length refusals. This removes the old text-only
cap within existing section and aggregate bounds. Complete chain acceptance
for its legitimate 7,670 effects / 3,635,865 bytes remains pending the combined
native acceptance. Public legacy recovery consumers remain separately tracked;
the current native builder does not need their BF phase certifier.

Earlier registry candidate P2 (source `6969dc7a`, SHA-256
`ef4a7aa34ada381c90435f98b10298aad4a9030e674cdc5cdc27ec103538ff4c`)
built pinned Tender `4cc58705` through its public entry in 63.822 seconds. Its
executable and two recaptures retained all 76,154 unique address rows with
identical row bytes. Their file/DATA growth led to `4e8a865e`, now repaired in M
and accepted on the application fixture. M built the same Tender workload
through its public entry in 63.216 seconds. Both recaptures succeed in about
2.1 seconds; all three files retain identical prefix/region/DATA extents and
all 76,271 unique address rows. The third generation passes Tender’s complete
standalone gate in 16.140 seconds: DOCX/XLSX extraction, filling, preservation
and refusals, plus REPL checked definitions and type rejection. Interactive
recovery remains explicitly untested by that gate. Evidence: `/tmp/cedar-tender-M/`.
Evidence: `/home/joel/.cache/cedar-capture-rows-u5l55np1/tender-P2/`.
The speed targets and downstream acceptance remain open; accepted
Tender/Maki/Kestrel pins stay unchanged.

Reader reuse reduces frozen OPEN calls per trivial definition from 2,895 to 328
while preserving generation/state/bounds validation on all 24,924 reads. Three
interleaved append-B/H pairs measured trivial AOT at 994/953, 994/953 and
996/961 microseconds, and three-operation AOT at 705/645, 707/640 and 707/641.
JIT remains 29–30 microseconds; every run counts exactly 200 NCOMP calls. Own
build lanes were drained, but an external Maki lint used about 34% CPU. These
are composed-product measurements, not isolated reader attribution or quiet
acceptance. Logs: `/tmp/cedar-H-reader-pairs/`. The 500-microsecond trivial,
uncached Tender and complete executable-build targets remain unmet.

The reviewed opcode batch (`54fad493`, integrated as `e73c85f6`) removes
380 repeated symbol checks per small compile. A matched M-source native pair
measured trivial 983/981/980 to 925/925/928 us and three-op 642/647/642 to
611/612/610 us, with exactly 200 optimizing compilations in every run. JIT
stayed 30–31 us. This is a measured 5–6% improvement, not the 500 us target.
The candidate SHA is `46a9d66884fafba495d309ddd1ca077fabee466798a10c31f3c9a14c7819ed3c`;
source and measurement evidence are in `/tmp/cedar-batch-M-pairs.json`.

## Required result

The optimizing compiler is itself optimized native machine code. It compiles
source directly into native code for every executable, including Habu itself.
AOT builds do not invoke the JIT compiler, stage bodies through JIT code, or
fall back to it on an error. Compile-time words, immediates and definers execute
as native code in the build host. There is no interpreted compiler in this design.

The interactive service uses the JIT. Preserve the previously requested ordinary
`hb --load` loader behavior; executable-build entries select AOT before loading
any tool or application dependency, even when launched through `hb --load`.
A test that disables JIT compiler entry during an AOT build proves independence;
it does not prohibit native execution or remove the product's REPL capability.

Completion means:

- Optimizing selfbuild and product-hosted rebuild, with no retained JIT-built
  compiler/application definitions, and no JIT fallback.
- Correct first-generation layout, artifact IO/merge, capture, restore, repeated
  capture and checked REPL behavior.
- Tender, Maki and Kestrel acceptance on an identified replacement toolchain.
  Preserve their accepted pins until their owners verify the replacement.
- All 3,079 definitions of the pinned Tender workload through the optimizer in
  under 1.7 seconds, uncached, with normal checks and validation. Trivial-definition
  compile time below 500 microseconds. Measure full executable-build time too,
  including loading, capture and writing; an internal pass time is insufficient.
- Required compiler/runtime suites green with meaningful behavioral coverage.

## Evidence and limits

Review base: `.jj-ws/rowan-root`, source `5226a994` plus tracker-only `4f234270`;
engine SHA-256 `28e11361f60228d24e7e3f6fca496ca0f9a4f2c8c8e66b5fdc91c9db031de462`.
Hazel reported 314 suites with seven failures. That is not a fresh green gate.

Previous combined gate: source `963958d8`, engine `bee6aacab6734`, ran all
318 suites with 12 failures: `effect-read-api`, `aot-wid-restore`,
`aot-wide-format`, `pre-trust-defer`, `build-fixpoint-fixtures`, `cast`,
`engine`, `engine-runtime-regressions`, `program-diagnostics`,
`multi-error-api`, `addrmap-inline`, and `p2-map-rewind`.
The provider matrix, owner window, internal-call guards, symbol ceilings,
local-case/environment regressions and new debugger-resume regression pass.
The runtime suite's new refusal is an invalid created-word effect in its wide
`does>` fixture; the corrected declaration reaches the intended rejection.
Private-checker fixture repairs are integrated at `1c4a890d`; the five affected
suite categories pass focused checks. They have not been counted as a new full gate.

Reviewed transient lifecycle source is integrated at `d90cf0a3`; ten focused
suites and Cedar's independent capture/concurrent-buffer checks pass. It is newer
than the combined gate above. Symbol indexing is integrated at `41c7af94`: fresh
source tests pass, with 774/1692/3197 successful lookup probes for 512/1024/2048
symbols. Typed owner dispatch is integrated at `a5b1956b`; the fresh tier1 owner
fixture passes. `A64IR-OPCODE:TAG` exposed two independent blockers: NFAM reads
the retained checker's registry after source-owner replacement, and the checker
drops match-layout facts beyond 24. Family-owner dispatch and private callback
bindings are integrated through `2783bbce`; independent fresh source loads pass
at both tiers. Match facts use the growing CWIN owner at `62147e15`; the actual
76-arm enum and widened construction execute, and superseded mappings are freed.
The private fixture binding is corrected at `48d5a612`. Native-match's emission
probe now selects AOT explicitly and restores the caller's tier (`200f43bc`);
both direct and gate-style loads pass with unchanged instruction assertions.
The old native-match test explicitly expected overflow rejection. Acceptance
now requires the compiler's actual 76-arm match to compile and execute.

Owned artifact sections are integrated at `2b94ee43`. Real captures survive erased
source sections and released source mappings; exact address rows and all payload
bytes return. Both tiers pass, as does the artifact suite on lifecycle host
`cdac89e02a34`. An overflowing positive section length is refused before copying.
The later source-bound writer and first-generation layout checks remain active.

Source-stage repair `3ed0b9d0` and immutable prefix boundary `32a21b6e` are
integrated. Empty-source and partial restored runtimes pass the boundary and
real rewind checks. Compiler-chain row validation is integrated through
`4aa70f9a`: exact location/kind/target checks, reordered-row acceptance and six
corruption refusals. A sorted index replaces the quadratic verifier; independent
checks pass at 32,768 rows. Two WID collision fixtures still need real target
collisions. No all-AOT product or application replacement is accepted yet.

The paired native bootstrap now compiles past the opcode enum. Remaining source
callable-boundary corrections are under test. The target writer review found a
retained driver defer that re-registered old heap storage after reset, and fixed
engine locations that still crossed unchanged when source layout moved them.
The driver now invokes checker operations through typed execution directly;
fixed-slot translation and capture validation remain in the layout lane.

Hazel's complete Tender pair was 153.3 to 131.6 seconds; the later session pair
reduced the trivial floor from 4,106 to 3,668 microseconds. Both used a compiler
whose implementation was JIT-compiled. No verified all-AOT compiler measurement
establishes the remaining factor. The 1.7-second target is not a promised result
of multiplying historical improvements.

Source review confirms repeated dictionary scans, duplicate combine planning
and quadratic address registration. Symbol indexing is integrated at `41c7af94`;
its earlier linear-lookup diagnosis is superseded. Residual spill scaling and
the payoff from pass-level reader reuse still need attribution.

## Native compiler and build design

1. **Local binding and checker rows.** Local names are case-insensitive and
   local-first in checker, JIT and AOT. Repair existing a16875d6, including
   different-case duplicates and global/package shadowing; execution must match
   certification. This existing P1 was omitted from the first reconciliation.
   **Provider rows:** Fix declaration publication. Quotation inference aliases
   named rows to implicit callback tails; publishing those inferred kinds changes
   the reusable declaration. This reproduces with call recording disabled, so
   `CALL-FREEZE` is not the primary cause. Preserve declared row quantification
   together with verified fixed cells, types and call-width facts; copying the
   original scheme wholesale would erase inferred input requirements.
   Quotation-wrapped `execute` reproduces the defect without `finally`.
   Cover direct/quotation/cleanup calls at both
   caller/provider tiers, empty/nonempty saved prefixes and wrong-type/borrow
   negatives. Do not alter valid `MEM:WITH-BYTES` effects to evade the defect.
2. **Arena append.** Check `from <= source-count` and
   `k <= source-count - from` before growing/copying in `APPEND-SPAN`. Preserve
   ownership/state checks, zero length at the end and unchanged destination
   on rejection. Never compute an unchecked overflowing endpoint.
3. **Host capture versus target emission.** `native-build.f` currently binds
   emitters to resident layout before loading source layout. Retained host
   reset/capture code must interpret live memory with the host ABI. Load corrected
   checker, target layout and runtime inside the window. Freeze declaration,
   code and signature/type membership, then persist target registry stores into
   target DATA before closing its final bound: USIGS-SNAPSHOT-PERSIST allocates
   at here even without growth. Run target preparation once (split SEAL's existing
   call as needed) and copy a complete immutable capture using the host readers.
   Only then reopen the native compiler session and compile target-bound emission
   tooling outside that captured value. Pass the owned value explicitly to it.
   Verify source-owner/compiler reuse after preparation, forced grown registry
   stores, and unchanged capture bytes while writer definitions are added.
   Remove the lexical call to the earlier host-bound emitter and
   ambient cross-instance `AOT-BUF` access. The layout leaf owns this interface;
   reuse existing capture sections/storage, without another format or disk stage.
   Keep capture storage live until emission finishes. Translate moved fixed
   engine slots by semantic identity for supported source-layout transitions;
   reject an unknown incompatible host before capture. Window offsets remain
   relative. Test growth/shrinkage and actual versus advertised heap/capacity
   boundaries in generation 1. Later convergence cannot excuse a hybrid product.
4. **Tier and owner.** Finish/review pending `.jj-ws/rowan-tier` at `91fff115`,
   preserving the definition-tier latch and source checker-owner record. Select
   executable-build mode before the first dependency, including direct APP-IMAGE
   use. Hold it through include/require/evaluate, generated words and immediates.
   A tier-0 request during that operation refuses before compilation. Retained
   callable JIT code also prevents saving; changing tier cannot convert it.
5. **Native bootstrap.** Use an identified runnable current native optimizer to
   compile the corrected checker and bind its source owner before providers load.
   If a bridge is necessary, compile the paired current checker/compiler using
   that optimizer. An ancient stdin seed, interpreter, fake setter or JIT rescue
   is unnecessary. After selfbuild, use the product as seed and remove the
   explicit pre-record by-name bridge.
6. **Interactive JIT.** Preserve direct native emission and checked call,
   quotation, loop and KEEP behavior. Delete the uncalled general machine-code
   copying inliner and tests requiring its existence; pending tier work already
   contains this deletion. Do not rebuild an optimizer inside the JIT.

## Capture and persistence design

- **Address rows:** complete eight-byte IO and exact-row verification are
  implemented (`0c1e78ca`, `4aa70f9a`); full changed-layout restore remains pending.
  Keep the shared width for lengths, bases, counts and merge. Preserve window/fixed
  location tags, CODE/DATA target tags and nullable offset-plus-one encoding.
  Shift only window locations and nonnull targets by their appropriate merged
  bases; check overflow before publication. Reject truncated rows and incompatible
  old versions. Test cleared-buffer reads, exact rows, both kinds, nulls, real
  merge and execution after restore. A write-read-write digest alone is vacuous.
- **Checker payload:** signatures/types are required for a restored checked REPL.
  Arm and mark the source-owner window explicitly and close before writer tooling.
  Restore against the correct registry base; exclude writer-only types. Do not
  delete empty sections to hide an unarmed producer. Define a family, restore it,
  reject a wrong-type call, and capture it again.
  The original signature text is insufficient: it loses verified variable kinds,
  inferred inputs, row tails and quotation/return relationships. Full native
  capture must explicitly retain the verified registry within target DATA.
  Partial artifacts must encode the verified graph in the existing signature
  rows/string section, preserving canonical constructor identity and sharing;
  reject older payload versions. Validate all graph references and registry
  bases before publishing any restored store. A textual reparse cannot substitute
  for the graph that was checked. Final preparation also detaches the previous
  compiler's tape callbacks and clears its process-local observer identity.
- **Transient storage:** use existing lifecycle preparation and the registry at
  both capture and snapshot entry. Register a control record on its first live
  allocation and unregister on release, so reserve after restore registers again.
  Declaration-only registration loses old buffers after `RELEASE-ALL`. Use a
  private membership handle/generation in the control record to avoid a new scan;
  reserve registry space before publishing allocation and update handles on
  removal. Release compiler transients after last use and before DATA copy.
  Writer storage stays live outside the captured value until writing completes.
  Delete only the per-pass release lists made redundant by this ownership.
- **Quotation storage:** declare relocation kind where the type is decided, even
  for an initially null cell. Add a mark-only code-cell operation paired with
  `ptr-cell-mark`, sharing the registrar with `xt!`. Generated typed DATA storage
  marks quotation fields; transient mapped callback storage remains unregistered
  and legal. This does not permit saving retained JIT code.
- **Capacity:** preserve every live address declaration in a growable owned
  vector. Keep image layout and heap boundaries stable; secure replacement
  storage before publication, detach backing storage before a rewind can retire
  it, and persist outside registration. Validate the snapshot schema before
  reading rows. Admit artifacts against actual encoded sections plus framing and
  alignment within the existing budget. The 75,900-row and actual allocation
  refusal controls replace assertions tied to the obsolete fixed ceiling.

## Compile-speed design

Keep landed session reuse, hash removal, allocator maps and verifier work.
Independent source fixes can start now; controlled speed acceptance uses the
verified all-AOT product once available.

1. **Dictionary:** expose the matched record already returned by `WLFIND:LENTRY`.
   `NDICT:WL-CANDIDATE` must not scan XREF to rediscover it. Preserve visibility,
   owner authority, ambiguity, shadowing, rollback and retired-wordlist latest-row
   fallback. Keep one dictionary index. Reuse authenticated binding results within
   a definition; do not cache by spelling or a recyclable record pointer alone.
2. **Symbols:** the private context-owned hash index is implemented (`41c7af94`).
   Keep exact collision checks, arena-generation checks and ownership through
   clone/release. The separate committed symbol/byte ceiling check on prototype
   clones remains pending; do not rebuild the index to address it.
3. **Word tables:** carry their interner, resolve spelling uniformly and enforce
   intrinsic binding at LOOKUP for both link kinds. Remove separate session
   spelling arrays and the duplicate gate. Keep binding memoization definition-local
   because package shadowing changes during a load.
4. **Combine/spill:** rebase/retest `172fc17d`: one combine plan for the exact
   input module, consumed once, preserving fold precedence and cleanup. The module
   already rebuilds at most once; the defect is repeated planning. Attribute
   remaining spill-frame visits. If its Boolean frame-need fixed point dominates,
   propagate once through frozen predecessor lists with a worklist. Preserve the
   necessary allocation/rewrite fixed point and existing allocator validation.
5. **Address registration:** index exact DATA byte offsets to existing ordered
   rows. Same-kind duplicates do nothing; conflicts/capacity refuse before stores.
   Rebuild/invalidate at restore, reset and compaction, including native-build's
   `ADDR-ROWS!`. Persist neither scratch pointers nor stale ordinals; unaligned
   cells are legal. Measure the actual cold-build contribution.
6. **Remaining reads:** measure pass-level reader opens after selfbuild. If material,
   retain one existing checked reader per input arena in the pass cursor and add
   reader-taking dialect operations. Rebind when the module changes; preserve
   owner/generation/state/bounds checks. This conditional follow-up belongs to
   the existing reader task, not a new raw-memory fast path.

Use existing Habu floor, per-definition and scaling tools. Record source/binary
identity, compiler provenance, uncached settings, dispatch count, wall time and
pass visits versus definition size. Quiet conditions are for measurements, not a
prerequisite to implement. Preserve the combine/spill slope target at most 1.1
on established workloads and verify the final total; operation counts distinguish
complexity changes from noisy clocks.

## Integration and delivery

Reuse dots, correcting stale diagnoses and idle ownership claims. Each leaf owns
named source sections and focused regressions. The layout, tier-provenance and
symbol-ceiling leaves establish their interfaces before consumer edits. Encode
all real dependencies; parallel work uses separate workspaces where necessary.
Split the pending stacks by these source owners before landing them; the first
leaf must not import another leaf's unfinished changes. A separate Astra reviews
delegated changes before landing.

Rebuild the exact source, run affected real-load suites, then
`bin/hb --load test/run.f`. Resolve every recorded failure by name. Replace
obsolete compiler-state assertions with their behavioral claim, preserving saved
real values across calls, KEEP through `begin ... until`, and quotation-body spills.

Native selfbuild must preserve checked internal-word refusal (a772a9a2), including
the first product. JIT must authorize internal calls even when a typed pointer
effect is available. Keep retired-token rejection while correcting the neighbor
fixture to use its actual pointer effect. Repair the
environment fixture's complete-value comparison (440084ec) without truncating
the inherited value or changing the user's environment.

Restore debugger watchpoint resume in native products (74e8b1d9): reduce the
first SIGSEGV after `2 WID .`, compare original and product-hosted engines, and
preserve watchpoint output, signal resume and subsequent REPL behavior.

JIT publication must preserve verified minimum inputs and types (d1bd23c6).
The safe compile-only `ROW-ADD ( R -- R ) 1 +` pointer-caller counterexample
currently rejects under AOT but passes under JIT. Reuse the verified effect
contract; ordinary loader/REPL routing remains JIT.

Reuse standalone delivery for joint acceptance: Tender's local `required` scanner
case and full runner closure; Maki's `GEOM:SHAPED-PAIR`, native build, warm capture,
restore and REPL; Kestrel's compiler and embedded target handoff. Report hardware
acceptance separately where hardware is required. Minimal CLI help is insufficient.

Make native-build accept a private output and have chain builds use it; never move
`bin/hb`. Add same-source two-build byte comparison to the existing chain tool,
including a dirty-transient negative, rather than creating a second framework.
Verify a product-hosted rebuild and convergence before candidate promotion.

Repair the supported source-reading stage path alongside this work: load/provide
stdlib once in its cold prefix, keep the bootstrap mirror consistent, and resolve
the actual target callee closure before emission. The eight unresolved seed names
must be supported literals or actual target definitions, with name/site diagnostics
on refusal. General JIT inlining and fake providers are not remedies. Certify the
assembled source and complete isolated no-binary recovery including the final native
refresh/product verification (CHECK_ONLY stops earlier at hb-stdin), after removing
the pre-record bridge; this route does not
block use of an existing native optimizer for selfbuild.

Finish bounded review findings too: quote native fixture paths with correct
argument-buffer capacity; publish/execute a namespaced generated constructor with
a conflicting same-tail name; exercise actual artifact reader refusals. Preserve
behavior while deleting obsolete test machinery.

## Exclusions and deferred work

PTX/GPU/model CAD and Loom policy belong in `~/Work/loom`. No new IR architecture,
general cache framework, verifier bypass or benchmark language is required here.
The historical IR/GPU plan does not add commitments to this campaign.

Keep the existing object-cache request as a later incremental-build task; it
cannot satisfy the uncached target. Defer the 75-ms-per-179-s source-digest
micro-optimization until whole-build speed is measured. Further hotspots need
evidence and an update to the owning task, not speculative new passes.

## Dispatch order

The dot tree is the detailed work graph; short IDs below identify existing leaves.
Current claims are recorded in dots. Cedar owns combined product acceptance and
shared DATA ownership. The registry lane owns synchronization and measured
registration cost; the checker lane owns diagnostic recovery without granting
source authority. The maker lane is determining which cold artifact fixtures
can use the current native builder, while preserving their behavioral claims.

| Result | Dependency path | Ownership boundary |
|---|---|---|
| Native selfbuild | rows → tier/owner1dc23a17 → layoutabdd0188 → routingcf2b21d4/payload eec26aea → selfbuildc348eab0 | Checker, tier dispatch, capture/emitter interface, entry routing; separate leaves |
| Symbol/binding cost | arena → ceilings615f47a9 → symbol indexa35dd84d → word table297b990d → record lookup2b13e978 | Interner contract before callers; one existing dictionary index |
| Combine/spill | lifecycle → ca192310 | Existing combine/spill algorithms and scratch only |
| Tender capacity/registration | layout → capacity1ca5db10 → registrar3c5f6d9b | Capacity contract before derived index; reset belongs to registrar |
| Capture acceptance | artifact/layout/row fixes → payload; lifecycle → quotation storagee92b0571 | Exact rows, checker payload and transient lifetimes each have one owner |
| Recovery and delivery | tier/artifact → stage2f64be7c → recovery29c5dc0b; private chain9fe66f8e → identity8d249e4d → deliverya86d4699 | Current-native selfbuild does not wait for no-binary recovery |
| Final speed | all-AOT baseline + speed leaves → reader attribution516b2416 → campaign gate | Existing timing tools; final uncached3079-definition result |

Small cleanup/refusal/constructor leaves remain explicit in the tree. Pending
tier91fff115, combine172fc17d, capacity0d78b97f and source-prefixb728e382 are
inputs to review/integration, not evidence that their acceptance has passed.

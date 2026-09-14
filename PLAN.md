# Finish Habu's native compiler

Status: implementation in progress, 2026-09-14. Cedar owns integration. This replaces
the obsolete IR/GPU migration plan, preserved in jj history. Reuse the existing
Tender campaign `habu-compile-the-tender-8385810f`.

## Current correctness work

Restart checkpoint: read [RESTART.md](RESTART.md) first. Cedar owns integration;
performance work is deferred by Joel's latest instruction.

Joel's current order is compiler correctness and the Kestrel backend handoff,
then TRUST/TRUSTED retirement. Do not add new unchecked boundaries while the
legacy migration waits. Current ownership:

- Cedar: native typed-load validation (38ad40e8), integration and qualification.
- Hazel: dynamic native +loop (db5978a3); warmed source-order verification
  (0c9fe3d7) follows that coherent patch.
- Astra literal_ownership: physical stack bounds, engine/layout/lifecycle
  (986147f9); native emitter and recovery parity follow the reviewed guard ABI.
- Astra closure_integration_review: remaining gate fixture/dependency repairs
  and the AOT bundle/data failure, with separate review before integration.
- Astra backend_handoff: read-only review of the shared compiler/target boundary
  (4e96cb27) and exact Kestrel handoff blockers.

The combined engine SHA8c1b0755 ran all 378 suites: 374 passed and four failed.
compiler-native-again's obsolete expectation is already repaired at2b09e979 and
passes focused validation. The remaining failures are aot-chain-capture's
pointer-row census, native-gate-aot-negative's missing SNAP-RELOC dependency,
and native-gate-aot-positive (bundle/data -8303 plus the confirmed missing
typed-load tag validation). No full green gate or release qualification yet.

The September audit targets an older branch and engine. Current revalidation
confirms quotation-input variance, implicit return-row reads, and trailing
signature tokens as blockers. Tested repeat/dead-tail cases and the listed
row-variable caller exploits now reject. Further audit claims remain to assess.

Maki accepted frozen source54e9ae7c + SHAc37b51ff for its full suite, plain and
all-Kiapi/native-cell images, restore, recapture and REPL. Its pin remains held
until Habu release qualification. That native engine rebuilt itself byte-identically.
Hazel's reviewed C2/C4/M8 fixes and the reviewed decimal fix are integrated through
cd82e4d9. Reviewed boolean correction 6594f822 rebuilds successfully as engine
954c53fd; focused checker, layout, float and native-finally tests pass. The internal
Astra integer-overflow agent implemented engine admission; Cedar implemented the
matching checker admission. Both and the float overflow/name-claiming follow-up
are reviewed and focused tests pass on combined engine SHA2a49a29c. Public ndict
count bounds (M3) are integrated at180b7e63 and included in engine SHA515efca8.

The default stripped route now selects the optimizing tier before dependencies
and retains EXECUTABLE-BUILD scope through source loading. Obsolete maker code is
removed through e9111ac2. C5 code-address relocation is committed at1948ba71:
native linker builds, fresh quotation images and malformed-chain refusals pass.
Dynamic object payload storage is integrated at1fb297ec. Seed DATA address-site
publication is repaired at7bc7d386 and survives two native generations. Persistent
literal ownership and linker re-interning pass on test engine SHA4ccbc973: Cedar
and Hazel independently confirm the actual MEM:UNMAP error image emits its exact
20-byte message, and mutable pre-window DATA still refuses without an image.
Guarded ownership is integrated atde322418, split WID cases at6d60ebba and normal
entry lookup atb91ad53e. The guarded native engine SHAc3ddf790 passes ownership and
entry fixtures; snapshot ownership passes. Combined rebuild/full gate is next.
No new release pair is qualified yet.
These are separate from Maki's accepted --repl images. Warm source-order
verification and public recovery remain open. Tender reports its previous
frozen9d pair passes standalone/REPL acceptance; Kestrel acceptance is pending.

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

## Deferred compile-speed design

Keep landed session reuse, hash removal, allocator maps and verifier work.
Resume this work after correctness acceptance; controlled speed acceptance uses
the verified all-AOT product.

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

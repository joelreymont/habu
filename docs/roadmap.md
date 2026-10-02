# Roadmap: Habu as the base for generated code

Joel's plan (2026-09-16): Habu is the one language in which AI generates all
the code he needs, on servers and on microcontrollers. This page names the
seven campaigns that stand between today's tree and that, what each is
grounded in, and how the tracker is organised around them. It is the map;
the campaign dots are the ledger.

## Design documents

- C2: [ownership-model.md](ownership-model.md), the reviewed scoped ownership
  contract and implemented native view, storage and cleanup surface.
- C5: the decisions appended to [database-models.md](database-models.md) and
  [tasking-models.md](tasking-models.md).
- C6: [x86-64.md](x86-64.md) and [cortex-m.md](cortex-m.md).
- C3 and C4 are specified by their child dots and by MISSING.md; C1 by
  PLAN.md and the release checklist.

## Tracker rules

- Under one hundred open dots. Every open dot is true of the tree today,
  concrete, and belongs to exactly one campaign or to the release line.
- A campaign dot is an epic. Its description lists the ids it absorbed when
  the tracker was rebuilt (2026-09-16), so `dot find` still reaches their
  text in the archive. Absorbed dots were closed with the reason
  `superseded by <campaign>`; done work was closed with its evidence; work
  for Loom or Etch was moved to their trackers.
- A child dot is dispatchable in under a day and carries Problem, Acceptance,
  Files, Verify, Depends, Ownership and Claim. Design decisions get a dot
  whose acceptance is a document section, not code.
- New work enters as a child of a campaign or not at all. A finding that fits
  no campaign is a reason to revisit this page, not to add an orphan.

## C1 Toolchain: finish the compiler and qualify a release

Remaining work is tracked by `PLAN.md` and the open release dots. Follow
[RESTART.md](../RESTART.md) to the current integration head and
[bootstrap.md](bootstrap.md) for the native build and recovery routes.
Qualification belongs to a particular source/engine pair and its recorded
gates; downstream acceptance is a separate part of the checklist below.

Grounding: `PLAN.md` "Required result" and "Dispatch order" define the
compiler's completion and its dependency graph over existing dots. The
release checklist defines release quality: full gate green on a
byte-fixpoint engine, the recovery chain reaching bootstrap check OK,
stripped applications green, snapshot suites green, silent traps closed,
downstream proof on Radar, Tender, Loom and Etch, docs current, integration
lines advanced and the root engine replaced.

Campaign content: the PLAN.md graph and the checklist are the children.
Acceptance for the campaign is the checklist closed plus PLAN.md's
completion list met, and every downstream repository building from the
released engine with no pinned pair.

## C2 Memory safety the checker can see

What exists in the source tree: shared and exclusive views carry lexical owner
and loan scopes; initialized records and tables retain borrowed field
dependencies. The public `C2-MEM` owner path appends zeroed allocations to a
named owner and accepts checked disposers. Task-local frames close on return,
throw and cooperative halt. `XML-C2` scopes a reader over shared source bytes.
Source, native and saved-image E2Es cover these paths; the exact source/engine
pair still needs release qualification. Legacy raw MEM pointers remain
lifetime-free.

Grounding: Tender's OPC cache publishes immutable bytes until package close,
two XML readers advance independently over them, and DOCX tree fields retain
source slices after the cursor closes. The final DOC owns copies and outlives
the package. These require distinct source and cursor lifetimes, typed borrowed
fields and helper returns, and independent mutable copies for writers.

The implemented core follows [the ownership contract](ownership-model.md):
checker views and dependency propagation, typed scoped storage and loans,
task-local cleanup, and MEM/XML adapters. The remaining consumer work is an
isolated OPC/DOCX/XLSX proof and deliberate downstream migration after a
qualified release. General linear locals, lifetime parameters on every raw
pointer, and the whole record/span migration are not prerequisites.

Acceptance: checked programs reject escaping views, overlapping mutable loans,
owner release with a usable loan, and authority recovered through stale cells,
raw storage, deferred state or task transport. The consumer proof preserves
shared sources, independent cursors, tree slices after cursor close and owned
documents after package close. Cleanup runs exactly once on return, throw and
cooperative task termination. Effects survive native image roundtrips, while
capture of live C2 scopes is refused. A candidate proof does not by itself
publish a public engine or require Tender to switch its engine.

## C3 The ergonomics traps

What is missing (`MISSING.md`, Tender LESSONS): locals have no defined
precedence against the dictionary, shadow words case-insensitively, and
cannot use natural names (Foundation B); quotations cannot see locals;
repeated structural types have no transparent alias; packages cannot nest.
Foundation A1 landed: `DEFTYPE` (`lib/type/deftype.f`) declares a nominal
integer in source, with its converter pair.

Campaign content: B: scope frames with innermost-first resolution, locals may
shadow ordinary words within their scope, shadowing a control word is a
located error, prototyped on a temporary engine and landed only at fixpoint.
Plus the existing alias and package-hierarchy dots, and the retirement of the
legacy `SUMTYPE` and `PRODUCT` declarers. Acceptance: the fixtures named in
`MISSING.md` for B pass and the rules an AI had to memorise are enforced
instead.

## C4 Diagnostics that name the fix

What exists: `docs/repair-diagnostics.md` is a stable JSON contract for
checker diagnostics with code, repair class, span and suggestion, gated over
fixtures; repair packets are built from it for LLM repair loops.

What is missing: several checker and engine paths `die` or crash on
user-reachable input instead of throwing a located diagnostic.

Campaign content: one child per observed failure. Acceptance: every child is
closed, each with the fixture that reproduces its failure.

## C5 Runtime services

What is missing: see [platform-gaps.md](platform-gaps.md),
[tasking-models.md](tasking-models.md), [socket-models.md](socket-models.md)
and [database-models.md](database-models.md).

Campaign content: the eight service dots of 2026-09-16 (contained worker
throws, blocking semaphore, typed join, messages and queue, TCP, generic I/O
devices, the database decision, the cooperative kernel decision), plus HTTPS
through `libcurl` over the FFI and task events later. Acceptance: a Habu
server accepts TCP connections in worker tasks, survives a worker throw,
talks HTTPS to an API, stores rows, and the same task vocabulary compiles
for a target.

## C6 Targets

What exists: native compilation for arm64 on Linux and macOS; instruction
constructors for ARM32 (ARMv7-R A32 and ARMv7E-M Thumb-2) and TI C6x with no
lowering behind them; serial and XMODEM device peers under `test/`; kestrel's
target registry dot so backends load only when used.

What is missing: an x86_64 backend (Joel is taking this on with an Intel
machine), lowering and image layout for the ARM32 and C6x encoders, and the
load-run-debug loop over serial that SwiftX calls the cross-target workflow.

Campaign content: the registry first; then a design dot for x86_64 (SysV ABI,
what of the arm64 selector is shared, ELF64 image builder reuse); then a
design dot for the first microcontroller target on the Thumb-2 encoders with
the cooperative kernel from C5. Acceptance: one program text compiles for
arm64 and x86_64 hosts and for one Cortex-M board, and can be loaded and
inspected over serial.

## C7 Learning material and hygiene

Closed. `docs/forth.md` and its card are what an agent reads before writing
Habu; `tools/public-signatures.f` prints the public signatures of the tree on
demand; `docs/stdlib.md` is the library guide. The stale plan documents and
document sections this campaign named are gone.

## Campaign work that was removed

Each row was planned under a campaign above and dropped after it was checked
against the tree.

| Work | Why it was removed |
|---|---|
| A generated library reference with a staleness gate (C7) | `tools/public-signatures.f` prints the true signatures on demand; checked-in pages need a regeneration in every library change |
| JSON diagnostics for runtime failures; one owner, one provoking test and a gate per error code (C4) | nothing reads a runtime JSON diagnostic, and no observed failure stands behind the sweep |
| Owner-only product construction (`CONSTRUCT owner`) | its only consumers are two proof tokens in Loom, and the flag value the dot reserved is `DRV-ADDR` |
| Binder heads on `ENUM` (`NAME<a,b>`) | no declaration needs them, and the dot depended on a `DECL-HEAD` package that is not in the tree |
| A mutation for every register-allocation verifier refusal | six of the 21 codes listed no longer exist, and `docs/proofs.md` does not ask for mutation runs |
| A new rejection for control nests deeper than 32 | already refused: a 33-deep nest exits 70 under `--load` and under `tools/check.f --all-errors` |
| Unifying every throw row of a quotation | `THROW-EDGE` already folds every throw edge of a body into the intact masks |
| Residency transfer for a trap's live operands (C6, x86 lane) | only hand-built HIR reaches the refusal: a source trap passes fresh literals and a source `die` lowers as `terminal` |
| A forged-artifact negative for every reader refusal (C1) | no failure stands behind them; the one defect found, a signed overflow in `AOT-FILE:?TABLE`, landed with its negative |
| A generator of FFI declarations from C headers (C5) | no consumer: `lib/pg.f` and `lib/net/curl.f` carry fifty hand-written `FUNCTION:` rows that work |
| `lib/process.f`'s single-descriptor polls moved onto the AIO loop (C5) | only test harnesses call them, and the capture loop under every child run stays on `poll(2)` either way |
| `TYPE-FIXES-PLAN.md` and `docs/tracker-rebuild.md` | a stalled plan whose rules contradict the tree, and an inventory nothing reads |

# Roadmap: Habu as the base for generated code

Joel's plan (2026-09-16): Habu is the one language in which AI generates all
the code he needs, on servers and on microcontrollers. This page names the
seven campaigns that stand between today's tree and that, what each is
grounded in, and how the tracker is organised around them. It is the map;
the campaign dots are the ledger.

## Tracker rules

- Under one hundred open dots. Every open dot is true of the tree today,
  concrete, and belongs to exactly one campaign or to the release line.
- A campaign dot is an epic. Its description lists the ids it absorbed when
  the tracker was rebuilt (2026-09-16), so `dot find` still reaches their
  text in the archive. Absorbed dots were closed with the reason
  `superseded by <campaign>`; done work was closed with its evidence; work
  for Loom or Maki was moved to their trackers.
- A child dot is dispatchable in under a day and carries Problem, Acceptance,
  Files, Verify, Depends, Ownership and Claim. Design decisions get a dot
  whose acceptance is a document section, not code.
- New work enters as a child of a campaign or not at all. A finding that fits
  no campaign is a reason to revisit this page, not to add an orphan.

## C1 Toolchain: finish the compiler and qualify a release

What is missing: `PLAN.md` is titled "Finish Habu's native compiler";
`docs/bootstrap.md` records that neither self-rebuild path can replace a
working engine; Tender and Radar run only on pinned engine and source pairs;
`RESTART.md` says not to call Habu release-qualified.

Grounding: `PLAN.md` "Required result" and "Dispatch order" define the
compiler's completion and its dependency graph over existing dots. The
release checklist `habu-qualify-habu-for-9ccd0432` on the hazel line defines
release quality: full gate green on a byte-fixpoint engine, the recovery
chain reaching bootstrap check OK, stripped applications green, snapshot
suites green, silent traps closed, downstream proof on Radar, Tender, Loom
and Maki, docs current, integration lines advanced and the root engine
replaced.

Campaign content: the PLAN.md graph and the checklist are the children.
Acceptance for the campaign is the checklist closed plus PLAN.md's
completion list met, and every downstream repository building from the
released engine with no pinned pair.

## C2 Memory safety the checker can see

What is missing: the checker proves stack effects and nominal types, not
lifetimes. Tender's OPC module frees intrusive buffer lists by hand; Tender's
LESSONS record locals overwritten by later pushes; `lib/task.f` and the AOT
capture path reuse buffers whose authority nothing tracks.

Grounding: the borrow and capture dots already on the tracker: immutable
lexical MEM borrows, lexical mutable scratch borrows, linear capture phases,
scoped memory and context cleanup (Cedar, active), pointer lifetime region
types, fixed DATA layout from a typed schema.

Campaign content: those dots, ordered scoped memory first, then read and
mutable borrows, then region-typed pointers, then linear phases. First new
child: write the ownership model into `docs/forth.md` and `docs/type-system.md`
before more code, so every lane implements one rule set. Acceptance: a
program that stores a scoped span past its owner's release, frees while a
reader lives, or reuses a phase buffer out of order is rejected at check
time, with fixtures for each; Tender's OPC lists are rewritten on the typed
surface as the proof.

## C3 The ergonomics traps

What is missing (`MISSING.md`, Tender LESSONS): nominal integer types are
engine constants, not declarable in source (Foundation A1); locals have no
defined precedence against the dictionary, shadow words case-insensitively,
and cannot use natural names (Foundation B); quotations cannot see locals;
repeated structural types have no transparent alias; packages cannot nest.

Campaign content: one child per foundation. A1: a `nominal` declaration
backed by a tag table, explicit converters generated, strictness identical
to the built-in roles, measured on Tender by the drop in explicit conversions.
B: scope frames with innermost-first resolution, locals may shadow ordinary
words within their scope, shadowing a control word is a located error,
prototyped on a temporary engine and landed only at fixpoint. Plus the
existing alias and package-hierarchy dots. Acceptance: the fixtures named in
`MISSING.md` pass and the rules an AI had to memorise are enforced instead.

## C4 Diagnostics that name the fix

What exists: `docs/repair-diagnostics.md` is a stable JSON contract for
checker diagnostics with code, repair class, span and suggestion, gated over
fixtures; repair packets are built from it for LLM repair loops.

What is missing: engine and runtime failures do not follow the contract;
several checker paths `die` on user-reachable inputs instead of throwing;
error codes are minted outside `lib/errors.f`; over a hundred codes are
provoked by no test.

Campaign content: extend the contract to engine and runtime failures (every
exit path prints the same shape with a code and a suggestion), replace `die`
with `throw` where a user can reach it, one owner for codes, a test per code.
Acceptance: a generated program that fails at check, compile, load or run
time yields one located diagnostic with a repair class, and the gate refuses
a new code without span, class and fixture.

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

What exists: `docs/forth.md` is the one document an agent must read;
`tools/public-signatures.f` extracts public signatures; `docs/stdlib.md`
declares itself the LLM-facing library guide.

What is missing: the reference is hand-written and drifts from the tree
(dots record `docs/forth.md` stating the opposite of what ships, and plan
documents contradicting the tree); nothing generates a library reference
from the typed signatures the checker already holds; the idiomatic corpus is
the library plus Tender and nothing else.

Campaign content: generate the library reference from public signatures and
the package doc comments, in the VFX DocGen manner, and gate it against the
tree; repair `docs/forth.md` and the plan documents; keep LESSONS as rules.
Acceptance: the generated reference is byte-identical across two builds of
the same tree and the gate fails when a public signature changes without its
doc.

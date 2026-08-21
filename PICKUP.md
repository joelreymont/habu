# PICKUP — development moves to a new machine (written 2026-08-21)

The previous machine (macOS arm64) is being wiped and replaced with Omarchy
(Arch Linux). Everything an agent needs to continue is in this repo or named
here. The per-machine memory files are gone; this document replaces them.

## Who you are and how to work

You are the ORCHESTRATOR. You plan, dispatch worker subagents into isolated
`.jj-ws/<lane>` jj workspaces, review every diff hunk by hunk, gate in a
dedicated `.jj-ws/merge-gate` workspace, and fast-forward `master` only on a
green you ran yourself. You never write implementation code. Master is always
green. Read `AGENTS.md` (a.k.a. CLAUDE.md — symlink), `docs/forth.md`, and
`LESSONS.md` completely before any work; they are the operating rules and the
accumulated scar tissue, and they are current.

Hard-won process rules (all cost incidents to learn):
- A verdict is READ, then the merge is a SEPARATE command. Never compound a
  bookmark move with a gate.
- Dots edits happen ONLY from a working copy parented on just-merged master.
- `PASS (cached)` is a verdict about a cache key, not a tree. Gate phases key
  on ENGINE-SET:FILES (binary + boot prefix via tools/boot-pin.f).
- A crossing landing (reader keywords, prefix reorders, package seals of
  prefix files) is ONE procedure per workspace: move the checkout to the new
  tree, THEN replace (rm + cp, new inode — macOS killed overwritten signed
  binaries; Linux may not care, keep the habit) the binary. Old engines
  cannot read crossed trees and vice versa; `cp bin/hb-host bin/hb` then
  `install --force` is the crossing build.
- Adjudicate host-load test flakes by named class + standalone rerun; the
  documented classes on the old host were proc-pty, native-match case 234,
  the runtime/tail ratchets, insn-proof standalone. ALL TIME BUDGETS ARE
  HOST-RELATIVE — expect a re-calibration round on the new machine.
- An open dot is a plan, not evidence. A worker's report is a claim until a
  measurement confirms it. Verify-first has saved 6+ merges.
- The dot CLI re-quotes `created-at` on `dot on` (bug, dot 3e6492f4) — repair
  by hand. `dot off` archives leaves into gitignored .dots/archive/ — close
  by editing frontmatter in place when the closure must publish. Dot leaves
  cap at 1024 lines (lint dies namelessly past it).

## Platform switch — do this first

1. Recover a working engine on Linux: `HABU_ALLOW_BOOTSTRAP=1
   tools/bootstrap.sh` (see docs/bootstrap.md) or build from the Linux path.
   `bin/hb` / `bin/hb-host` in any checkout are macOS arm64 binaries — dead
   weight on Linux; rebuild both.
2. Expect these to need re-measurement on the new host: every wall-clock
   ratchet (test/gate-*-lib budgets), the size-attribution Linux rows
   (dot habu-the-sparse-window-3368bb76 already owes Linux legs: the
   aot-data-span-forge PTY cases and the −5560 attribution split), and the
   gate calibration lines.
3. The judge (`tools/judge.f -- --check`) compares against a committed
   baseline measured on macOS; byte columns should agree, clang reference
   columns may differ — read tools/judge/report.f's no-compiler policy.

## Repo state

- `master` = `81d88a3a` on origin, GREEN under the engine-keyed gate.
- Bookmarks pushed for pickup:
  - `route3-banked` = `2c865355` — the LIVE route-3 chain (7 commits):
    boot-pin packaged, the type-foundation block moved past the check hook,
    95 TRUSTED: forwarders deleted, publication-is-an-act mechanism
    (IMPLEMENTATION/;IMPLEMENTATION/API in util.f + internal-mark.f), the
    SCHEMA-REG unit (37 API marks under a written criterion, self-check,
    lifecycle contract — SEE OVERTURN BELOW).
  - `route3-ref-a-shape` = `4f1b44b6` — superseded pre-seal derivation, kept
    as reference only. Never rebase or reuse directly.
- Workspaces (`.jj-ws/habu-trusted`, `habu-thecut`, `habu-effstore`,
  `merge-gate`) die with the machine — recreate with `jj workspace add` as
  needed. All lane knowledge lives in dots leaves, not workspaces.

## The campaign (read these leaves first)

Standing orders: (1) THE CUT — checked native chain becomes the sole colon
compiler, then delete the old one (dots habu-cut-colon-compilation-a5aa3f1f,
habu-delete-the-old-679cfd35). (2) BEAT CLANG. (3) TRUSTED elimination
(epic habu-trusted-dies-prim-4fd12d60): no ledger, PRIM axioms only for the
foreign bottom, everything else checked. src TRUSTED census when route 3
lands: 218 → 123.

The GOVERNING VISIBILITY MODEL — every clause user-ruled, forced by
measurement, history on the leaves:
1. Private by default.
2. Extend by reopening: sibling implementation files reopen the owner
   package pre-seal and define narrow entries where privates are in scope
   (worked specimen: TFAM:DERIVE-SET in sumtype.f).
3. Seal at the boundary: prot-wid-add ceremony; C-PACKAGE-SEAL-GUARD then
   refuses reopen (SCHEMA-REG and TYPE-DECL sealed; TFAM/LOWER-CERT closed
   via the RESTAB reserved-name table).
4. Publish on purpose: the IMPLEMENTATION/API declaration in util.f;
   internal-mark seals undeclared names (interpret route residue = 0,
   proven fail-closed: a new definition in a declared file is refused with
   no other edit).
5. Public means public — to checked code too. The checked-route closure is
   the LATE RETIREMENT PASS: after the prefix finishes (placement is a
   load-order fact — internal-mark.f is last by construction), every
   unpublished name is retired from the dictionary via existing
   UNDEFINE-FOUND. Compiled prefix references stay bound; user code gets
   E-UNDEFINED. Built and probed (SCHEMA-REG: 61/63 closed, survivors =
   the 2 API-marked); pass source at leaf §16 and in the (dead) session
   scratchpad — re-derive from the leaf if needed.
6. API is decided by a CRITERION, not by usage: contract (constructors,
   front doors, doctrine-required error codes) = API; bases, cursors,
   mutators = implementation regardless of callers; borderline = listed for
   ruling. The criterion is written at the top of each file's mark block.
7. LATEST OVERTURN (user): NO lifecycle save/restore API at all. Its only
   clients were test suites; isolation-by-child-process is the tree's own
   idiom (RUN-LOAD children). SCHEMA-MARK/SCHEMA-RESTORE (in route3-banked's
   tip commit) must be DELETED; registry-polluting suite cases (the
   REG-MARK/REG-RESTORE fixtures in enum-decl-suite/structure-decl-suite,
   the TWX-*-RESET pairs) convert to child programs; internal rewind
   machinery (COUNTS/REWIND, TFAM-REWIND, PFX-MARK/PFX-REWIND,
   SCHEMA-ROLLBACK-*) stays private and untouched.

## Route 3 — the resume spec (dot habu-route-3-the-64078d43)

THE LEAF IS THE COMPLETE SPEC (~1000 lines, at the 1024 cap — trim
superseded before adding). Resume point:

1. Start from `route3-banked` (2c865355). First commit: execute overturn §7
   above — delete SCHEMA-MARK/RESTORE (two of the 37 marks go with them),
   convert the suites' registry-polluting cases to child programs, re-print
   the census.
2. TFAM unit: marks under the criterion (borderlines listed), the
   TWX pair conversions, then the RETIREMENT PASS over TFAM + SCHEMA-REG
   together. The one deliberately-held red (type-field-owner-suite assert
   188) clears here when PF-COMMIT-N retires; strengthen TF-CELL-PUB →
   TF-CELL-PRIV in the same commit.
3. TYPE-DECL unit, including tools/decl-gen-probe.f converted to an
   extension block inside the package (instruments are implementation
   tooling; no API marks for screwdrivers).
4. Closing census: residue 0 on BOTH routes (interpret + checked-body probe
   over every package public), API = declared surface only.
5. Full battery + the crossing procedure (this is a crossing landing), then
   merge. Gates: install --force fixpoint twice byte-identical, full
   test/run.f zero FAIL/RED, maki/test.f, judge --check 46 rows,
   schedule-lint 0 unreached, both diff lints, error-code-lint,
   dot-dep-lint, bootstrap-mirror-lint, snapshot suites, recovery leg.

Known stop-classes on the way (all documented on the leaf): census must be
SCOPE-aware not file-granular; names resolved from strings (schedule-lint
matches GSI-* AS TEXT; child-program sources; GE-FILES: is itself a definer
form in tools/lint/def.f); engine-bare resolution (s" literals in src/habu,
bootstrap/cg/forth.fs; AOT-captured callees — rc 82 is the signal).

## After route 3 lands (the queue)

- The visibility phase of TRUSTED elimination closes; remaining phases on
  the epic leaf: foreign class (checked words over the axiom'd trampoline —
  NO FOREIGN: form without a failing probe, dot bc70057e), the hard core
  (c0507484), test premises (dc125344), then delete TRUSTED: from the
  reader (feac682b).
- THE CUT phases B–E (capacity ceilings c86fb5cc — scouted and ruled, leaf
  has the derived-caps plan; census-reach 859ea853; then route the sole
  compiler and delete the old one 679cfd35).
- Gate-cluster packaging campaign (habu-pkg-lib-test-06230d06 — fully
  banked: first leg diff, ring-3 census, def.f-driven requirement, three
  silent-green string classes; the runner.f lint exception row retires with
  it).
- Smaller: four blind comparators (c8ddf040), record-header dedup
  (0ca98eef, 652KB measured), binary type-info encoder (c6a3d0ff,
  unblocked), hb-host build stamp (f8ee29d3), claim-liveness lint
  (6796f28e), reopen defect general case (113ecd89), pre-hook TRUSTED
  inert (a2a2b5e7), GPT-2 device preconditions (5db2072c), stdin-inheriting
  runner fixtures (ae3b87f6), tick-of-undefined (421f6ec0 + fold),
  subsystem-visibility capability (92764584, closed-unnecessary, probe-gated).
- External-review epic (5cb4522c): holes 4–7 still unverified (SSA edge
  args, effect-token domains, tensor bounds, sched-key toolchain identity)
  — verify-first, refutation counts as delivery.

## Tracker

~2048 dots, ownership layer now truthful (a GC pass closed provably-landed
leaves, released 99 dead claims, swept 349 satisfied blocker edges, fixed
the timestamp corruption). `dot ready` works. The claim choreography is in
AGENTS.md; close-in-place when closures must publish.

## The one-sentence philosophy

Nothing in this tree is true by accident — not a public name, not a green
gate, not a census, not a tracker claim. Measure before acting, probe before
building, stop at boundaries, and land nothing that isn't true.

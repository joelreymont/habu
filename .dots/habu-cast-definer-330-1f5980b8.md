---
title: "Cast definer: 330 nominal casts want one declaration form"
status: active
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.378520+02:00"
---

Phase 2 of 4fd12d60: 330 TRUSTED: sites are nominal identity casts - 232 with literally empty bodies, 98 pure stack shuffles (roles.f 34 - which GENERATES the TRUSTED: text at roles.f:40 - process-pty-handle.f 18, cad-num-types.f 12, maki/ 111, tail 96). Build a checked cast-declaration form: a definer that states from-type/to-type and mints the identity with the checker enforcing representation-compatibility (same cell count/roles), replacing trust with a structural check. The roles.f generator then emits the new form. Blocks the final deletion.

Probe lead (2026-08-19): a CAST: definer already exists - tools/judge/cost.f:155
uses `CAST: REAL-BITS ( r -- n ) ;`. Find its definition and semantics FIRST;
the capability may be extending CAST: with representation-compatibility
enforcement rather than minting a new form.

Claim: agent=trusted-5 workspace=.jj-ws/habu-trusted

PROBE ANSWER (2026-08-19, trusted-2). The capability is already built and
already tested; nothing was added. `CAST:` is defined in src/core/roles.f and
certified by src/core/checker.f CAST-CERTIFY. It is a STRUCTURAL check, not a
trusted spelling: the checker certifies the body under the identity row
( in -- in ), publishes the declared ( in -- out ), and refuses the declaration
itself with a named code when it lies about shape - 7129 E-CAST-ARITY (any cell
count other than one in, one out), 7130 E-CAST-CLASS (pointer, quotation, atom
or width>1 term on either side), 7131 E-CAST-FAM (undeclared family), 7135
E-CAST-OWNER (minting a cell family this package does not declare), 7137
E-CAST-LINEAR (either side may carry linear ownership). A cast may therefore
rename a cell and may never reshape one, which is exactly the phase-2 contract.
Coverage already exists: test/cast-suite.f (positive) and
test/cast-negative-suite.f (all five codes).

MEASURED GAP vs TRUSTED:. TRUSTED: catches a shape lie only where some caller's
row happens to disagree, and blames the caller. Proven on master: `TRUSTED: L1
( n -- idx idx ) ;` with an empty body hands the caller its own pre-existing
stack cell retyped as idx (probe printed 5 then 77, then E-UNDERFLOW), and
`TRUSTED: L3 ( n -- ptr u8 ) ;` lets `8 L3 c@` compile and SIGSEGV the engine
(rc 134). Both are refused by CAST: at the declaration, by name.

DONE: lib/cad-num-types.f, 12/12 sites (10 private MINT-* plus 2 private
projections). Suites unchanged and green; a shape-lying mint now kills the
build (7130 measured).

BLOCKED, NEEDS A RULING: src/core/roles.f, 0/34. `CAST:` must RUN at
declaration time and it crosses source through sumtype.f's deferred
TDECL-EVAL-XT, which src/core/include.f arms. roles.f is prefix row 894 and
include.f is row 902, so a single CAST: in roles.f dies "defer: unset execution
vector" (rc 76) - measured with one converted pair. Arming the boundary inside
roles.f makes all 34 convert and the engine build clean (measured), but that
mints a SECOND audited evaluate crossing and arms generation eight files early,
which is the exact fail-closed property sumtype.f:923-927 says it is keeping.
A chain reorder cannot fix it either: include.f needs ZBYTE@ from env-base.f
(row 901), env-base.f sits after roles.f, and roles.f's consumers bytes.f
(>LEN/LEN>N) and os/linux/layout.f (>VA/VA>N x6) sit between - measured by
putting include.f first and getting E-UNDEFINED: ZBYTE@. Note also that
converting roles.f breaks the INSTALLED engine at boot, so any landing needs two
steps (reorder, install, then convert). Options for the orchestrator:
 (A) arm a second evaluate boundary in roles.f - one trusted wrapper replaces 34
     trusted casts, but weakens the single-crossing invariant;
 (B) give CAST: a pushback/next-definition window so it delegates to `:` on the
     live stream and needs no evaluate at all - best long-term, removes the
     generated-text fidelity caveat too, but is a checker.f CAST-PEND protocol
     change;
 (C) move roles.f + bytes.f + os/linux/layout.f after include.f - env-base.f and
     habu/layout.f use no roles and no bytes.f words, so an order exists, but it
     touches five mirrored lists (habu2.f, build-fixpoint.f, boot-pin.f,
     diagnose-hb-core.f, bootstrap.sh) and still needs the two-step landing.
Also found: the "generator at roles.f:40" in this leaf's description emits no
declaration. DECL-SIGNATURE is handed the name span and the signature span
separately, so the `TRUSTED: ` / ` ( ` / ` ) ;` literals were never read by
anything. They are deleted in this lane's roles.f hunk - no behaviour change.

SWEEP DONE (2026-08-20, trusted-5). The load-position caveat is dead: master
8818e2cb made `cast:` an engine reader keyword that works at any load row, so the
sweep is textual everywhere and no file needed a chain check.

Scope was 143 single-line empty-body TRUSTED: rows outside test/ (pty 18,
maki/ 90, tail 35). The leaf's "maki/ 111, tail 96" counted bodied rows that are
not casts at all - MK-SPAN drops a cell, the EMIT-* ops run real work, the
MINT-*-PROOF rows take nothing and push a literal - so the shape census was
about 35% optimistic, the same trap LESSONS records for the trust census.

63 converted, 60 refused BY THE CHECKER, 20 certified by the checker but blocked
by the PACKAGE LINT. Nothing was forced. Tree census 1360 -> 1297 TRUSTED:,
121 -> 184 CAST:. test/ untouched (phase 7, dc125344).

              converted  checker-refused  lint-blocked
  pty                12                6             0
  maki/              30               45            10
  tail               21                9            10

NO SHAPE LIE WAS FOUND IN THIS SCOPE. Measured with EFFECT-DIN-CELLS: a `ptr a`,
`ptr n` or `ptr u8` term is ONE cell (`ptr u8 n` is two), so every refused row is
already cell-count honest and its empty body really is the identity. Each refusal
is a POLICY refusal, not a caught lie:
  - 7137 E-CAST-LINEAR, 9 rows: the retype would mint or erase a use-once token.
    process-pty-handle.f's six lifecycle tokens, KV:batch's KB-MINT/KB-TAKE,
    safetensors SESSION>CENSUS. Correct to refuse; they keep their retirement
    owners, and the pty file now says so in one line with the measured code.
  - 7130 E-CAST-CLASS, 19 rows: a pointer on one side, which is a class
    reinterpret. The GPU buffer/session and safetensors mint/take pairs,
    lib/task.f, lib/json-read.f, lib/ffi-abi.f, src/compiler/native/string.f.
  - 7135 E-CAST-OWNER, 32 rows: a package introducing a value into a family
    another package declares. 26 are the maki id-mint pattern (RAW>*-ID into
    CAD-KIND from packages SCHEMA / ARTIFACT / REV / CONFIG / MAKI / MIR / ...),
    6 are PTX register mints inside package ATTN and package GPT2-ATTN. Where the
    matching projection could be converted the pair is now split - a checked cast
    out, a trust row in - which is the honest split, since the mint is where the
    authority actually lives.

FINDING 1, needs an orchestrator ruling: the 26 maki id-mints are ONE
architectural question, not 26 chores. Either package CAD-KIND owns the mints -
which destroys the per-package unforgeability those files' own comments rest on -
or each package declares its own id family. That is a public-interface decision
with a caller cascade; no worker should improvise it. lib/ptx/cg.f's nine
equivalent register mints certify only because they sit at global scope, which is
the same fact seen from the other side. Precedent: lib/type/extent-role.f.

FINDING 2, the harder one, and it is NOT about casts. Twenty rows the checker
certifies could not land because tools/package-diff-lint.f refuses the EDITED
LINE for a pre-existing reason:
  - E-PACKAGE-OWNERSHIP, 10 rows: lib/ptx/cg.f's nine register mints and
    lib/ptx/tile-v4a.f V4-ALIGN define outside any package.
  - E-REDUNDANT-PACKAGE-PREFIX / -FILE-PREFIX, 10 rows: ARTIFACT-ID>RAW in
    package ARTIFACT, and its nine siblings in CONFIG / ACTION / EVIDENCE /
    OBLIGATION / RUN / PRODUCER / REV / SCHEMA / TARGET.
Proved pre-existing by mutation on PRISTINE master: adding a trailing comment to
the untouched `TRUSTED: SPAN-REG` line raises the same E-PACKAGE-OWNERSHIP, and
the same trick on `TRUSTED: ARTIFACT-ID>RAW` raises the same prefix reject. So
the lint has frozen those twenty DEFINITION lines - any edit to the line itself,
not to the comment above it - until somebody makes a packaging decision for the
global PTX codegen surface, or runs a ten-name rename cascade across ~20 maki
files including their tests. Both are outside a sweep's contract. The twelve
files carry a note above the frozen rows saying they certify and why they did
not move.

Left for phase 7 (dc125344): test/ 737 rows, untouched here.

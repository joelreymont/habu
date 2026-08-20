---
title: "Route 3: the type foundation loads post-hook, checked"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-19T10:53:19.909085+02:00\""
blocks:
  - habu-typed-storage-sweep-b2cd1a61
---

The zero-trust route for the 485 recording-gap TRUSTED: sites (fab55650's blocked set, epic 4fd12d60): src/habu/habu2.f:861-890 loads type-schema.f, type-family.f, sumtype.f and checker.f BEFORE src/core/check-hook.f installs the hook, so their : definitions record no signature - measured 2026-08-19 by trusted-1 through the real prefix build (ED-PROBE rc 70 evidence on the leaf of fab55650). Move the type foundation post-hook so its signatures are DERIVED AND CHECKED - the bootstrapping knot is the checker checking its own foundations, which is the same territory as habu-seal-the-checker-5314c0ab (package ownership of checker.f) and the dissolved umbrella habu-tfam-2b-sealed-1b77662c (re-derive before dispatch). Rejected alternatives, with reasons on fab55650's leaf: owner-side declared-signature recording (trust that route 3 would delete), mass PRIM axioms (contradicts the epic by its own text). This blocks fab55650's remaining 485 sites and therefore feac682b (the reader deletion).

SCOUTED VERDICT (2026-08-20, full map in the scout report; re-measured):
1. THE KNOT IS ONE EDGE, NOT FOUR FILES: check-hook.f's closure is checker.f
   alone (0 hits into the 825 type-file names). The only real obstruction is
   render.f -> type-family (13 token sites, 5 load-bearing in REND-SIG's path)
   - render.f must stay pre-hook because it installs RECXT, the ONLY
   inferred-effect row producer, whose default silently discards signatures.
   Cut with the existing checker defer wall (checker.f:465-482/:896-907,
   bound at type-family.f:3218-3239) - proven in-tree instrument.
2. SCOPE: move type-schema.f, type-family.f, sumtype.f, layout-buffer.f,
   layout-valid.f AS ONE BLOCK to immediately after the hook row (zero-length
   window; the last two have zero pre-hook consumers and cost nothing).
   checker.f CAN NEVER MOVE: its 129 forwarder sites (70 distinct words) are
   conceded to PPRIM: axioms - the tree's own stated preference
   (prefix-rewind.f:41-43, LESSONS.md:204-209). Route 3 unblocks 352 sites,
   not 485; the ruling says so.
3. REPLAY-AT-HOOK IS STRICTLY DOMINATED: it still requires the 662 definitions
   to typecheck and adds either route-1's declared-text carrier (rejected) or
   a second CHECK pass over the same bytes (second authority).
4. PRECEDENT PRICED: structures.f moved across the hook 2026-07-15
   (ksxvzllqmnks) - 14 files, 11 ordered manifests, the exact churn list is in
   the scout report.
5. HARD GATE BEFORE ANY MANIFEST EDIT: run tools/check-core.f over the three
   files and COUNT THE REJECTS. 662 definitions have never met the checker.
   Encouraging: near-zero raw-pointer surface in schema/family. Discouraging:
   their post-hook peers run 35/44 and 39/55 TRUSTED:-to-definition ratios.
   If the owner-side bill approaches that, route 3 nets far less than the
   epic assumes - re-open the ruling on that number.

Claim: agent=route3-1 workspace=.jj-ws/habu-trusted

HARD GATE ANSWERED (2026-08-20, lane route3-1). ALL 662 DEFINITIONS
TYPECHECK. Measured through the real driver, tools/check.f -> check-core.f
(hook-sites row 6), staged in the prefix order route 3 proposes, each file
carrying the preceding ones as cross-file support:
   bin/hb --load tools/check.f -- --json-errors --all-errors --source-list \
       <preamble> ./src/core/type-schema.f ./src/core/type-family.f \
       ./src/core/sumtype.f
Counts, per class (a) clean / (b) honest type error / (c) capability gap /
(d) probe artifact:
   type-schema.f    62 defs   62 clean   0 type errors  0 gaps
   type-family.f   411 defs  411 clean   0 type errors  0 gaps
   sumtype.f       189 defs  189 clean   0 type errors  0 gaps
There is not one honest type error and not one capability gap in the block.
Every reject the probe ever printed was class (d) and each was traced to its
cause, not waved away:
 - The driver SKIPS a canonical prefix path at expansion, preverify,
   all-errors and child-run alike (CHK-DEP-PRELOAD? -> REQUIRE-KNOWN?, an
   exact byte compare against the engine-provided registry), so
   `check.f --source-list src/core/type-schema.f` exits 0 having checked
   NOTHING. Spelling the same file `./src/core/...` restores it as real
   input. This generalises LESSONS.md:784 - the failure is silence, not the
   E-UNDEFINED that lesson predicts.
 - sumtype.f's own `PRIM: NEWTYPE/SUMTYPE/PRODUCT PRIM;` axioms are already
   in the parent's checker registry, so re-checking the file dies
   `checker: duplicate definition` at line 1819. The probe renames those
   three heads and their three axiom rows, nothing else; mutating each of
   the three bodies is still caught, so all three are really checked.
 - src/habu/xref.f:673 `undefine TFAM-PKG-XT` retires, far downstream of the
   block, the defer that type-family.f:3220 binds. Restoring exactly
   checker.f:469's declaration in the probe makes TFAM-HOOK-INSTALL certify.
   That was the LAST surviving reject, and it was ours, not the file's.
   Cross-checked: TFAM-PKG-XT is the ONLY prefix-undefined name the block
   references.
OWNER-SIDE BILL: 86 distinct rows - 83 on checker.f, 1 each on cell.f,
util.f and render.f (TDECL-DIAG, the one render edge the scout named).
64 rows copy a signature the owner already declares, 13 are the effect the
compiler itself publishes for `variable`/`create`, and 9 are signatures that
DO NOT EXIST in the tree today and have to be written (TAG, PAY, MK-CON,
MK-VAR, MK-ROW, MK-PARAM, MK-QUOT, CHECKER-STEP, USIGS-COPY, TKF, CON-OF -
all headerless checker.f definitions).
THE RULING DOES NOT REOPEN - THE GATE ARGUES THE OTHER WAY. 86/662 = 0.13,
against the siblings' 35/44 = 0.80 and 39/55 = 0.71, six times lighter. And
those sibling bills are mostly THIS defect: 27 of structure-decl.f's 35
TRUSTED: rows and 31 of enum-decl.f's 39 are one-token forwarders into
type-family.f / type-schema.f (FAM-DECL -> TFAM-DECL, SCH-ROOT@ ->
SCHEMA-ROOT@, ...). Route 3 pays 86 rows on checker.f and deletes 58 of them
outright in its two direct consumers, on top of the 352 sites the epic
counts. Tractable, and the number the scout feared is evidence FOR the move.
ONE DESIGN NOTE FOR WHOEVER TAKES THE MANIFEST EDIT: 13 of the 86 rows hand
out `-- ptr a` raw mutable cells of checker.f's private state (OK, FAILSET,
MULTI-ERR-N, SI, SL, TKFU, HIDX-GEN, PARAM-SCR-N, FIELD-FAM, CTOR-PEND-*),
because the block pokes those latches directly. A checker-effects.f that
exports raw storage handles is the shape master 0cc8d823 is about; decide
whether those 13 become accessor words before the rows are written.
Falsified, not asserted: a planted `( n -- n ) drop ;` at the FIRST and the
LAST top-level point of each of the three files is reported (so the pass
covers the whole file - this is how the NEWTYPE collision was caught);
dropping any sampled owner row brings back its exact E-UNDEFINED (16/16);
`is` against a TRUST row rejects while `is` against a real `defer` of the
same effect certifies; and a wrong hand-written row is not free - reading
`( -- t )` out of MK-QUOT's trailing comment forged a spurious reject until
the signature was read off the body instead.
ONE HONEST LIMIT ON THE NUMBER: the 64 copied rows repeat headers that are
themselves pre-hook and have never been verified, so if a header lies the
measurement inherits the lie. That is not a probe defect - it is the same
text a TRUSTED: row would carry, so the bill is exact for what route 3 would
actually write. The harness lives in the lane scratch (route3-gate3.py,
route3-falsify.py, route3-symidx.py); no production file was touched.

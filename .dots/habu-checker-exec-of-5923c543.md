---
title: "Checker: execute-of-stored-xt launders unsafe definers from checked bodies"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-13T17:24:38.523281+02:00\""
---

Static invariant: a word whose runtime body invokes an unsafe definer (deftype/value-record/layout-buffer/...) must not certify with a benign stack effect; the checker must not model an unknown executed xt as a pure ( -- ) quotation when that xt's true effect is unknown or unsafe. Where enforced: src/core/checker.f RSEXEC (~1557-1587). Problem (found by symset destruction review 2026-07-13): 'variable XV  ' deftype XV !  : VF ( -- ) XV @ execute ;  VF newtype' certifies VF as ( -- ) but at runtime runs deftype, parsing the live input and minting a type — generalizes to value-record and layout-buffer. Direct '['] deftype execute' correctly rejects (the immediate-tick operand carries its type); the xt's retyped/unsafe effect is LOST when it round-trips through !/@, so execute treats it as pure. The unsafe-symbol set (habu-checker-unsafety-as-1c537c1f) cannot gate this: at the execute site there is no symbol identity to key on. Needs xt-provenance-through-memory tracking (a memory-cell carrying an xt must retain a bound-effect/unsafe taint, or execute of a non-literal xt must be modeled conservatively/rejected in checked bodies). Acceptance: minimal negative fixture (the VF example + value-record + layout-buffer variants) rejects in a checked body; direct-tick and existing checked-quotation-through-'is' cases stay green; a legitimate stored pure xt executed from a checked body still certifies (no false-positive on ordinary deferred words). Files: src/core/checker.f (RSEXEC + xt/variable effect modeling), test/internal-word-gate.f or a new negative regression, docs/effects.md. Verify: red-first fixtures, native fixpoint byte-identical, boot-pin, test/run.f. Depends: relates to habu-typed-top-xt-096a8f1b. Ownership: checker RSEXEC / executed-xt effect modeling. Claim: unassigned.

PROGRESS 2026-07-14 (xt lane, merged 73cf3cf6): the DIRECT-TICK half is closed -
['] <unsafe-definer> now rejects on the candidate path, and ['] A execute
fit-checks A's real certified effect (xt<effect> via BTICK-TOK + RSEXEC
unification; test/xt-effect-test.f v1-v9). REMAINING (this dot's core,
unchanged): the stored-xt round-trip - variable XV ' deftype XV ! : VF XV @
execute ; - no symbol identity at the execute site; needs xt-provenance-through-
memory (taint/bound-effect on xt-carrying cells) or conservative rejection of
non-literal executed xts in checked bodies, per the acceptance already written.


RCA COMPLETE, RE-SCOPED 2026-07-15 (xtprov lane, no commit - honest BLOCKED):
the surgical RSEXEC rejection is UNSHIPPABLE alone. Erasure point proven:
variable/create cells are TVK-RAW with per-occurrence-freshened address schema
vars (E-INST, checker.f ~4400), so the xt<effect> bound at the `!` occurrence
never reaches the `@` occurrence; RSEXEC's T-VAR branch then models the unknown
xt as a pure row-preserving quotation (checker.f ~1655-1663). Typed cells
cannot carry xt<E> today: CHECKER-STORAGE-INFO (checker.f ~2706) explicitly
rejects quotation cell types. Red-first proven for all three definer variants
(deftype mints a live type from a certified ( -- ) word; value-record identical;
layout-buffer throws 7121 at runtime). BLAST RADIUS measured by instrumented
build: ~36 legitimate checked words use the identical raw-var @ execute shape -
the checker's entire plugin-dispatch architecture (tfam-resolve*/match-*-tok/
checker-* hooks, lbuf/tdecl eval hooks, habu1 fprim*/source-hook/etc.) - and
the blocking BF-CERTIFY scan rejects checker.f's own hooks, so a bare rejection
cannot build. No discriminator exists between VF and the hooks. SEQUENCING (this
dot flips RSEXEC LAST): (1) capability dot habu-typed-xt-cells: xt<effect> as an
admissible cell type (extend CHECKER-STORAGE-INFO + TYPED-VARIABLE/TYPED-BUFFER;
persistent monomorphic cell type, @ recovers xt<E>, execute fit-checks); (2)
migrate the ~36 hooks to defer/is or typed xt<E> cells, fprim* classified as
TRUSTED machine-code boundaries; (3) THEN flip RSEXEC's T-VAR branch to a named
E-EXEC-OPAQUE-XT reject with the three negative fixtures, keeping xt-effect
v1-v9 green; STATUS.md count updates honestly. COORDINATE: steps 1-2 touch the
checker.f/type-family/layout-buffer regions tfam's active lane is editing - do
not dispatch until that releases or with explicit region coordination.

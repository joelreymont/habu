---
title: Multi-error checking in core CHECK
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:34:55.584129+02:00"
---

HOOK throws 70 at the first rejected definition (src/core/check-hook.f:9), aborting the load (include.f:184 die): one diagnostic per process run. Multi-error reporting lives OUTSIDE core in tools/check-all-errors-core.f re-driving CHECK-CANDIDATE! per definition - which diverges from the load path (see habu-fix-sig-clobber dot). For LLM repair loops one-error-per-run is a first-order iteration tax. Fix: on reject emit diagnostic, reset DCUR/RCUR to declared rows, continue the definition; add a load mode recording rejects and continuing to next definition (fail-closed exit at end); retire the external re-driver. Complements habu-evaluator-result-obj-2cf9b484.

## Progress (native load-path multi-error mode — DONE)

Added an opt-in multi-error load mode to the checker core so the ordinary load
path itself collects one diagnostic per rejected DEFINITION in a single process
run, instead of aborting at the first reject. Off by default, so the fixpoint
build and gate keep the fail-on-first-reject behavior unchanged.

- checker.f: `MULTI-ERR` mode flag + `MULTI-ERR-N` reject counter, with
  `MULTI-ERR-BEGIN` / `MULTI-ERR-END` ( -> reject count, clears mode) / `MULTI-ERR?`.
  In CHECK, a rejected definition (verdict 0) in this mode counts the reject AND
  records its DECLARED signature via CHECKER-USIG-CERT-ADD, so later definitions
  check against a known effect instead of cascading undefined-word errors.
- check-hook.f: in multi-error mode HOOK returns -1 (a non-zero hook return
  publishes the definition; zero unpublishes it) so the name resolves for later
  definitions; CHECK already emitted the diagnostic and counted the reject. The
  body is compiled but never run on a check-only load. Default mode still throws
  HOOK-CHECK-RC on the first reject.

Fail-closed exit: the driver reads MULTI-ERR-END and exits nonzero iff any reject
was recorded; only verdict-0 rejects count, not verdict-1 uncheckables. Root
cause is not masked — diagnostics are emitted in load order as each definition is
checked. Test in test/engine-suite.f: three definitions where two reject and the
third calls the first and certifies against its trusted signature; asserts the
reject count is 2 and the mode clears.

## STATUS: CLI rewiring BLOCKED on a missing native capability + a policy decision

RCA (empirical, on the current tree). The re-driver (check-all-errors-core.f)
and the native MULTI-ERR load produce the SAME rich per-def JSON (both route
through the same DIAGXT/render machinery — schema_version, code, repair_class,
word, offending token, declared/inferred effects, suggestion are identical), so
the diagnostic-richness half of the dot's premise holds. But two differences make
a naive rewire break the byte-exact goldens (test/golden/diag-all-errors.err and
the check-cli / gate-engine slices), so it CANNOT preserve the output contract
as-is:

1. POSITION FRAME (the hard blocker, breaks diag-all-errors.err on its own).
   The re-driver reports FILE-relative line/column/byte_start/byte_end. It tracks
   def boundaries with source-lex and passes each def's file line/col/byte to
   `VERIFY:SOURCE-BUF-AT-IN-SCOPE` -> `SOURCE-AT!` -> `DIAG-ORIGIN!`, which sets
   the diagnostic origin `DIAGL0/DIAGC0/DIAGB0` (render.f JABS-LINE = DIAGL0 +
   JLINE - 1; JABS-BSTART = DIAGB0 + FAILB). The native MULTI-ERR `include` load
   never re-points DIAG-ORIGIN! per definition, so JLINE/FAILB stay
   DEF-BUFFER-relative: a 2-def file reports the 2nd def as line 1 (not its file
   line) and byte_start is short by the `: ` prefix. Golden diag-all-errors.err
   pins file-relative positions (GDX-AE-BAD1 line 3 byte 100, GDX-AE-BAD2 line 4
   byte 131); the native path emits line 1/def-relative. To rewire safely, the
   MULTI-ERR load must thread each definition's FILE position (line/col/byte at
   the `:` token) into DIAG-ORIGIN! before that def is checked. That position
   lives in the COMPILER's input-tracking during `include`, not in the checker —
   wiring it in touches the compiler load path (src/habu/habu1.f / habu2.f), and
   habu2.f is currently OFF-LIMITS (a sibling worker is refactoring CLI parsing
   there). See minted dot habu-native-file-relative-e0438cd1.

2. CASCADE POLICY (contract conflict for cross-def references). The native mode
   DELIBERATELY certifies a rejected def's DECLARED signature
   (CHECKER-USIG-CERT-ADD) so later defs check against it and do NOT cascade;
   the re-driver grades each candidate in isolation and REPORTS the cascade
   (a later def calling a rejected one is emitted as E-UNDEFINED /
   unknown_rejection). Empirically: `: BADA (n--n) drop ; : BADB (a--) dup ;
   : GOODC (n--n) BADA ;` -> re-driver emits 3 diagnostics (incl. GOODC
   E-UNDEFINED), native emits 2 (GOODC passes on BADA's trusted sig). The native
   no-cascade behavior is arguably better for LLM repair loops, but it CHANGES
   the reported error set. The current diag-all-errors golden uses only
   independent defs so it would not catch this, but check-cli / repair-hints
   fixtures with cross-def references would. Reconciling requires a project-level
   decision the dot does not make: either keep cascade-reporting (undo the
   predecessor's trust-the-declared-sig design) or re-baseline the goldens to the
   no-cascade behavior (violates "goldens must stay green"). Not a unilateral
   worker change.

Because preserving every consumer's output contract is a hard requirement and
both gaps are unresolved (one needs a new checker/compiler capability in
partly-off-limits territory, the other needs a policy decision), the rewiring is
left OPEN rather than forced into a contract-breaking change. The re-driver stays
until (1) lands and (2) is decided. No superseded code deleted (nothing is
superseded yet).

## Intra-definition recovery — NOT SAFE as the dot specifies (left open)

The dot says "reset DCUR/RCUR to declared rows, continue the definition." That is
insufficient: a mid-body rejection leaves far more than the two cursors dirty —
`OK`, `UNCK`, the fresh row vars `BROW`/`RBROW`, the unification bindings DCUR/
RCUR were unified into (TVT/RVT), the arena push cursor SPN, and the control-flow
frame state (LINBEF/LIN-TOTAL, #CFC/LMODE, DEADP/THSET/XSET). Resetting only
DCUR/RCUR and continuing would emit cascading FALSE errors (or mask real ones)
from the stale unification/control state. A sound intra-def recovery needs a full
per-token checkpoint/restore of checker state — exactly the state-restoration
machinery that habu-trail-based-unification-84a86c0c is about (TRIAL-SAVE/REST +
the scalar/registry snapshots). Blocked on that; no regression can prove the
narrow reset safe because it is not. Reassess after the trail work lands a
reusable per-token save/restore.

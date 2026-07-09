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

## Decision packet: CLI rewire unblocked except for the cascade POLICY (2026-07-07, head 11fbedbb)

BLOCKER 1 (position frame): RESOLVED by habu-native-file-relative-e0438cd1
(closed) - MULTI-ERR-ORIGIN!/MEO-APPLY thread file-relative positions with no
habu2.f edit; test/engine-suite.f:783-799 pins byte-for-byte golden parity
with the re-driver. Re-confirmed at this head: the native path emits the same
rich JSON through the same DIAGXT/render machinery.

BLOCKER 2 (cascade policy): re-confirmed empirically at this head with the
dot's own fixture (`: BADA ( n -- n ) drop ; : BADB ( a -- ) dup ;
: GOODC ( n -- n ) BADA ;`): re-driver emits 3 diagnostics (GOODC E-UNDEFINED
cascade), native emits 2 with reject-count 2 (GOODC certifies against BADA's
trusted declared sig). NEW FIXTURE CENSUS RESULT that changes the cost picture:
NO existing fixture or golden pins cascade-reporting - surveyed
tools/check-all-errors-test.f (all sources independent or true-undefined
`NOPE`; the dup case is separate, below), test/golden/* (diag-all-errors
independent defs; diag-undefined true-undefined; repair packets single-def),
and test/gate-diagnostics-lib.f / gate-diagnostics-all-strict-lib.f (all
single-def or unrelated-def fixtures). Adopting no-cascade re-baselines ZERO
existing fixtures.

RECOMMENDATION (Option A): adopt the native NO-CASCADE contract. (i) It is the
real load path's semantics - the whole point of retiring the re-driver is
ending load-path divergence (the sig-clobber class); (ii) it is better for
repair loops: the cascade E-UNDEFINED on GOODC is a phantom (BADA is not
undefined - it is rejected), and phantom errors misdirect repair; the true
error set is minimal per iteration and re-running after repair surfaces
anything real; (iii) zero re-baseline cost today (census above). Add a NEW
committed fixture pinning the contract: the BADA/BADB/GOODC source must yield
exactly 2 diagnostics and no GOODC entry. Option B (keep cascade-reporting)
requires undoing CHECKER-USIG-CERT-ADD-on-reject in checker.f (item-8 lane)
and contradicts that design's stated intent - not recommended.

NEW BLOCKER FOUND + TOOLS-SIDE ANSWER (duplicate definitions): the re-driver
reports a duplicate definition as a diagnostic (CA-DUP-RC) and continues; the
native load path HARD-EXITS 78 at the duplicate (engine raw exit, not
catchable - conversion to a throw is the routed habu-raw-exit-compile /
BTHROW-family engine work). The rewired driver therefore keeps a light
def-name duplicate PRE-SCAN (names only - no per-def re-drive): duplicates are
reported as diagnostics and the driver fail-closes WITHOUT evaluating when any
exist, preserving the CAE-DUP contract with no engine change.

REWIRED DRIVER DESIGN (tools/check-all-errors-core.f, tools-only): replace the
source-lex def-splitting + per-def VERIFY:SOURCE-BUF-AT-IN-SCOPE re-drive
with: read file -> dup-name pre-scan (above) -> DIAG-FILE! label ->
DIAG-JSON!/DIAG-BUFFER! as today -> MULTI-ERR-BEGIN -> MULTI-ERR-ORIGIN!
(buffer base, data-base DEF-TKA-CELL + [the one engine-cell-read, top-level
interpreted setup per the e0438cd1 precedent - no new trusted site], 1 1 0) ->
evaluate -> MULTI-ERR-END -> exit nonzero iff count > 0. Diagnostic ordering:
load order (same as today). JSON: unchanged - one object per reject through
the shared render path (proven identical). RETIRES: CA-DEF-* vectors, the
source-lex def-boundary tracking (except the dup-name scan), the per-def
candidate-scope machinery in this tool. Suite deltas: CAE cascade fixture
added; everything else keeps passing unchanged per the census.

INTRA-DEF recovery half: unchanged - blocked on reusable per-token
checkpoint/restore (trail machinery); not part of the rewire.

STOP: awaiting the cascade-policy ruling (Option A recommended). On A, the
rewire above is a tools-only unit executable immediately; checker.f is not
touched under either half of this packet.

## PACKET AMENDMENT (2026-07-07, during rewire execution): evaluate-based
## rewire is UNSOUND for this tool - corrected design needs verification

RULING RECORDED: Option A (no-cascade) accepted by the coordinator.

But implementation investigation found a contract difference the packet
missed, discovered while auditing the 21 consumers' API surface:

1. EXECUTION SAFETY (disqualifying): the native MULTI-ERR path is a real
   `evaluate` load - it EXECUTES top-level candidate forms in the checking
   process. This tool's input is untrusted LLM-generated code; the re-driver
   is crash-immune BY DESIGN (per-def candidate scopes, nothing ever runs).
   An evaluate-based rewire turns a crashing candidate into a checker-driver
   crash (and in check-core's batch drive, one bad file kills the whole
   multi-file run). The engine-suite MEO regression got away with `evaluate`
   because its fixture is trusted test code.
2. DICTIONARY ISOLATION: CHECKER-SCOPE rollback (RBF-POP, checker.f:6267)
   restores checker registries (usigs/norets/syms/types/vrecs + TFAM hooks)
   but NOT the compile dictionary - RBF.NEND is NORET-END, not ndict. An
   evaluate-based core in a long-lived process (check-core drives many files;
   gate-dictionary calls CHECK-ALL-ERRORS-BUF repeatedly) would pollute the
   live dictionary per call and hard-exit 78 on cross-call name collisions.

CORRECTED DESIGN (to verify before implementing): MULTI-ERR x
VERIFY:SOURCE-BUF - wrap the existing check-only scope verifier (the same
path certify uses: parses + checks every def in a candidate scope, compiles
and executes NOTHING) in MULTI-ERR mode so a reject counts + records the
declared sig and the verify loop CONTINUES to the next def. Open questions
that need empirical verification and possibly touch src/habu/verify-source.f
(engine-family, item-8-adjacent): (a) does VERIFY:SOURCE-BUF's driver loop
continue past a rejected def when MULTI-ERR? is set, or does it abort on the
CHECK verdict; (b) do the MEO file-relative origins thread through the verify
path (verify has its own SOURCE-AT!/DIAG-ORIGIN! per-def frame - likely YES
via VERIFY:SOURCE-BUF-AT-IN-SCOPE, which the re-driver already uses); (c)
does CHECKER-USIG-CERT-ADD-on-reject fire on the verify path so no-cascade
holds there too. If (a) needs a loop change in verify-source.f, that is a
small engine-family edit to route; (b)/(c) are likely already true.

The dup-name pre-scan and the fail-closed exit from the original design
stand. The evaluate-based CLI sketch in the previous section is WITHDRAWN
for the core; it remains valid only for single-shot trusted-input contexts.

STOP (again): the corrected rewire hinges on (a)-(c); verifying them and any
verify-source.f loop change is the next unit. No consumer-visible code was
changed in this investigation.

## Probe results (a)-(c) + exact verify-source.f spec (2026-07-07, head c8419a37)

(a) NO - the loop change is REQUIRED. Source: VERIFY-BODY (verify-source.f:285)
`CHECK-BODY dup -1 <> IF 70 throw THEN` throws on any non-certified verdict
unconditionally (same in VERIFY-DOES-BODY :292). Transcript (cascade fixture
BADA/BADB/GOODC via MULTI-ERR-BEGIN + catch VERIFY:SOURCE-BUF + MULTI-ERR-END):
`catch-rc=70 reject-count=1`, stderr shows only bada - counted by CHECK, then
aborted by the verify loop.

(b) YES - byte-exact, MEO not needed on this path. Transcript (golden-shaped
4-def fixture via CHECKER-CANDIDATE-SCOPE-START + VERIFY:SOURCE-BUF-AT-IN-SCOPE
with 1 1 0 + JSON diag buffer): BAD1 emits `"line":3,"column":30,
"byte_start":100,"byte_end":103` - the exact diag-all-errors golden numbers;
the verify path's own per-def ORIGIN! (TOKEN-START, :519) threads file-relative
positions from the buffer origin.

(c) YES - declared-sig trust fires on the verify path; no-cascade holds.
Transcript (two buffers, one scope, MULTI-ERR on): verify BADA-only ->
`bada-rc=70` (diagnosed + counted), then verify GOODC-only in the same scope ->
`goodc-rc=0` (certifies against BADA's trusted declared sig), `count=1`.

PROPOSED verify-source.f CHANGE (small, engine-family - awaiting declaration
after the item-12 RECORD-DEFINER? conflict check):

  : VERIFY-BODY ( -- )
     BODY-BUF BODY-U @ CHECK-BODY {: v:n :}
     v -1 = IF EXIT THEN
     v 0 = MULTI-ERR? and IF EXIT THEN
     70 throw ;

(and the same three-line pattern in VERIFY-DOES-BODY). Semantics: certified
continues as today; in MULTI-ERR mode a verdict-0 reject continues to the next
definition (CHECK already emitted the diagnostic, counted MULTI-ERR-N, and
cert-added the declared sig - probes b/c); verdict-1 uncheckable STILL throws
in both modes (a file of only-uncheckable defs must not exit 0 - MULTI-ERR-N
counts verdict-0 only, so continuing on 1 would be fail-open). Default mode is
byte-identical. Continuation point is clean: VERIFY-BODY runs at the `;` token,
so the outer scan resumes at the next definition.

Once that lands, the tools-only rewire proceeds exactly as amended: dup-name
pre-scan -> DIAG-FILE!/JSON -> MULTI-ERR-BEGIN -> CHECKER-CANDIDATE-SCOPE-START
-> VERIFY:SOURCE-BUF-AT-IN-SCOPE (1 1 0) -> SCOPE-DONE -> MULTI-ERR-END ->
fail-closed exit; cross-file support keeps VERIFY:SOURCE-BUF-IN-SCOPE replay
unchanged (check-only, proven by probe c's same-scope behavior); the CA-DEF-*
re-drive retires. New fixtures: the 2-diagnostic cascade contract (Option A,
ruling recorded) + dup pre-scan + golden position parity.

## CLI-rewire half CLOSED (2026-07-07, on 6a1d8f3e) - dot remains open for
## intra-def recovery ONLY

Landed in one unit:
- src/habu/verify-source.f (declared for this change): VERIFY-BODY and
  VERIFY-DOES-BODY continue past verdict-0 rejects under MULTI-ERR (fail-open
  rationale for keeping verdict-1 uncheckable fatal stated at the site);
  checker-registry publication gap bridged by TRUSTED: MULTI-ERR-MODE? (the
  registry does not publish checker-internal words to later checked loads -
  discovered when the engine rebuild rejected the direct MULTI-ERR? reference).
- tools/check-all-errors-core.f: the per-definition re-drive is RETIRED
  (958 -> 483 lines): one whole-buffer VERIFY:SOURCE-BUF-IN-SCOPE pass in
  MULTI-ERR mode (CA-MULTI-BEGIN/END trusted mode control) inside the existing
  checker scope with unchanged cross-file support replay; duplicate definitions
  still surface as the catchable CA-DUP-RC from the verify path and report
  exactly as before (no lexer pre-scan needed - that concern was
  evaluate-design-specific); lex-unterminated detection kept; fail-closed exit
  iff rejects or rc. CA-DEF-*/CA-SUP-* tables, per-def diag frames, support
  slicing, and the raw-undefined JSON synthesis all deleted (the native render
  path emits every reject's JSON, byte-identical goldens).
- Fixtures: CAE-TEST-CASCADE pins the Option-A no-cascade contract (proven RED
  against the re-driver: 3 diagnostics incl. phantom E-UNDEFINED; GREEN at 2
  after the rewire); CAE-TEST-UNCHECKABLE-FAILS pins the all-uncheckable
  nonzero exit.
PROOFS: full CAE suite ok; check-test / check-repair-hints / repair-packet /
repair-schema-doc ok; build-fixpoint-test ok (verify-source certify pin);
fixpoint install --force rc 0 through blocking certify; serial prop oracle
census OK; cold gate PASS (one -2502 load flake, rerun green); trust-lint +
trusted-inventory strict green with the three new documented boundary rows
owned here.

REMAINING SCOPE OF THIS DOT: intra-definition recovery only (blocked on
reusable per-token checkpoint/restore machinery, as analyzed above).

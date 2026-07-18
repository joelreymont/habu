---
title: V2 differential runner core
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:27.511089+02:00\\\"\""
blocks:
  - habu-v2-differential-suite-2d896ced
---

Implement isolated deterministic differential execution for one scalar checker suite and one tensor forward suite. Store every input/output/environment, compare under declared domain, minimize discrepancies without replacing the original, and emit evidence or structured counterexample diagnostics. Acceptance: injected mismatch minimizes and replays, timeout/crash is distinct from numeric mismatch, PyTorch reference adapter remains outside Habu semantics, and success evidence is subject/suite/environment keyed.

Claim: agent=difftensor workspace=.jj-ws/fable-difftensor (tensor forward suite + subject-source injection + DIFFSUITE DECODE + suite-id registry with the sanctioned cad-kinds suite-id nominal granted this claim)

SCALAR CORE LANDED 2026-07-18 (diffrun lane, commit e1b397cb; claim
RELEASED). Package DIFFRUN, zero trust: typed defer execution vectors
for subject/reference; CLASSIFY-OUTCOME maps process outcomes so FAULT
dominates and never aliases wrong-value (constructed-outcome tests PLUS
real bin/hb children: hang -> SIGKILL-reaped timeout -> faulted, die 7
-> faulted); CLOSE? composes NPOL:dom + suite tolerance; MINIMIZE is a
pure deterministic shrink preserving the original (distinct CASE-ID);
success evidence keyed subject||suite-digest||env (flip matrix proven);
counterexamples lower losslessly to DIAG (round-trip proven). All four
acceptance legs proven for the SCALAR suite. LESSONS: bye exits 70 -
spawn subjects signal success by natural completion.
REMAINDER (this dot stays open): the TENSOR forward suite (float
elementwise comparator per the ort-ref pattern + tensor subject
adapter); subject-source-injection protocol (spawn adapter currently
proves isolation with an identity subject); the real PyTorch spawn
behind DIFFRUN_TORCH (interface + recorded skip landed); the folded
DIFFSUITE structured DECODE; the durable suite-id registry (blocked on
a sanctioned cad-kinds suite-id nominal - grant the one-line kind with
the next claim); a durable per-case output store (run-log is a bounded
first slice, LOG-CAP=64).

TENSOR + INJECTION + DECODE + SUITE-ID LEG LANDED 2026-07-18 (difftensor
lane, workspace .jj-ws/fable-difftensor; NOT yet merged/pushed). Zero new
TRUST in the checkable logic (the only new TRUSTED: are the mandated
§23.9 suite-id owner refinements RAW>SUITE-ID / SUITE-ID>RAW, which come
with the sanctioned suite-id nominal; both manifested in TRUSTED.md +
refine-lint seed 59). Per item:
 1. TENSOR forward suite (maki/db/diff-runner-tensor.f, reopens package
    DIFFRUN). Float elementwise comparator T-CLOSE? folds the ort-ref
    `f- fabs tol f<=` over SUBJ-T/REF-T (the maki EX-OUT fixed-buffer
    convention); composes NPOL:dom (exact -> zero bound -> elementwise
    equality; approximate -> the suite u64 tolerance read as a
    fixed-point bound in units of 1e-9 via TOL>F, so 10000 = the 1e-5
    ort-ref floor). Reuses the scalar core WHOLE: run-result/ref-result/
    case-verdict/run-verdict sums (n payload = element count), the
    bounded run-log, CLASSIFY-OUTCOME (fault dominates), and suite-level
    EMIT-EVIDENCE. T-CASE-VERDICT (fault>skip>compare, length disagree =
    mismatch not fault), T-FAILS?, T-MINIMIZE (least [0,p], original
    preserved via distinct CASE-ID), T-RUN, T-EMIT-COUNTEREXAMPLE (first
    mismatching element index + subj/ref floats lowered to a lossless
    DIAG, class numeric). All four acceptance legs proven for the tensor
    suite with constructed outcomes (maki/db/diff-runner-tensor-test.f)
    AND a real spawned bin/hb child (item 2 file).
 2. Subject-source injection (maki/db/diff-runner-inject.f). SUBJECT-SRC!
    stores a checked `: SUBJECT ...` source; INJECT-SCALAR / INJECT-TENSOR
    compose it with a harness binding the case input; SPAWN-INJECTED /
    SPAWN-TENSOR-INJECTED spawn+classify isolated. Refactored the base
    spawn adapter to a shared SPAWN-CAPTURE ( -- outlen clean) behind both
    PARSE-INT (scalar) and the new PARSE-TENSOR (folds whitespace-separated
    float stdout into SUBJ-T). Proof (maki/db/diff-runner-inject-test.f,
    REAL children): injected `2 *` vs `dup *` at case 5 -> produced 10 vs
    25 (different subject -> different classified outcome); injected `i` vs
    `i*i` tensor subjects -> element 2 = 2.0 vs 4.0; dying injected
    scalar/tensor subjects -> faulted.
 3. Folded DIFFSUITE structured DECODE (maki/db/diff-suite.f + tests in
    diff-suite-test.f). decode-result sum (ok<suite>|malformed|noncanonical
    |bounds|unknown); DECODE folds the canonical TLV envelope back through
    the SAME staged builder, resolves foreign ids (subject/comparison/
    target) via X:WIRE>KEY (unresolved -> unknown), places raw-key fields
    verbatim, re-SEALs, and gates the recomputed digest == stored tag-13
    digest (mismatch -> noncanonical). Proven: ENCODE|>DECODE round-trips
    to an identical-digest suite (order-independent); truncation ->
    malformed; tampered digest tail -> noncanonical; corrupt subject key
    -> unknown. Owns -5407..-5410.
 4. Durable suite-id registry (maki/db/diff-suite-id.f, package SUITEID)
    on the sanctioned `TYPEFAMILY suite-id 0` (added to maki/cad-kinds.f).
    REGISTER interns a sealed suite's DIGEST-INTO content key -> CAD-KIND:
    suite-id (equal suites, any order, share one id); EQUAL?, KEY>WIRE/
    WIRE>KEY (resolve by content), the evidence.f/run.f content-key-intern
    precedent. Owns -5412..-5414. Test maki/db/diff-suite-id-test.f.
 5. Real PyTorch spawn behind DIFFRUN_TORCH: torch ABSENT on this host
    (`python3 -c "import torch"` -> ModuleNotFoundError). The landed
    TORCH-REFERENCE interface + off-device recorded-skip (spawn test)
    stand unchanged; NO torch installed, NO faked result.

FIXTURES ADDED: maki/db/diff-runner-tensor.f, diff-runner-tensor-test.f,
diff-runner-inject.f, diff-runner-inject-test.f, diff-suite-id.f,
diff-suite-id-test.f; DECODE in diff-suite.f + diff-suite-test.f;
SPAWN-CAPTURE refactor in diff-runner-spawn.f; `TYPEFAMILY suite-id 0` in
maki/cad-kinds.f; two TRUSTED.md rows + refine-lint seed 59; error codes
-5407..-5414.

GATES (all green in-workspace): tools/dot-dep-lint.f (0), host-lint (0),
filemap-lint (0), error-code-lint (0), trust-lint (0), refine-lint (60
mints, 0), trusted-inventory (unclassified 0), stale-status-lint (0),
typed-local-diff-lint (0), the 3 new suites + diff-suite-test DECODE +
the existing diff-runner suites (test: ok), maki/test.f (green),
test/gate-stdlib.f (green).

OPEN / BLOCKED: the 3 new suites are gated STANDALONE (all green) but are
NOT wired into the monolithic maki/test.f. maki/test.f peaks at
ndict=16284/16384 on master (DICT-CAP=16384, layout.f) - only 100 free
word slots; the item-3 DECODE additions ride in via the already-registered
diff-suite-test.f and fit (peak now 16347/16384), but the ~148 words of
the three new core+test suites overflow the cap ("hb: dictionary full at:
:" at eval-device-fault-test.f). Wiring them in needs a DICT-CAP bump in
src/habu/layout.f (a precedented "gate-runner-support" growth, per the
layout.f comment), which is OUT OF SCOPE for this claim (no src/*
changes). BLOCKED: raise/land a DICT-CAP bump (or a per-suite forget in
the maki/test.f framework) so the three suites join the monolithic gate.
STILL OPEN on the dot: a durable per-case output store (run-log remains a
bounded LOG-CAP=64 first slice).

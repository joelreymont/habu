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

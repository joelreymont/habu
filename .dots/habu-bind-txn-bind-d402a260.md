---
title: Bind transaction BIND dispatch
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T18:09:25.424095+02:00"
blocks:
  - habu-add-wstore-scoped-e57e32e2
---

The SAFET mapping prerequisite landed 2026-07-27 as e0b22bf2 "Make SAFET
mapping detach total" and fa96f47f "Make mapping scope total", so
habu-return-typed-mem-ac35e3c9 is closed and its blocker edge is removed here.
The scoped weight-store read prerequisite remains.

Redesign 2026-07-27: the candidate and its former claim are rejected evidence
only. Resume only after the remaining blocker lands. BIND stays a thin exhaustive
dispatcher over one `mcfg`; mapped CHECK uses allocation-free SAFET
`moved|empty`, and both arms prove bytes through the committed model's
`WSTORE:WITH-RESIDENT-SLOT` path and the real allocated table. No late mapping
allocation, recovery state, copied slot walk, private arena bypass, optional
real-artifact success, or masked disposal result is accepted.

Claim: released after rejection of `666a7269`.

S6b4, the final bind-transaction leaf (rev-4 Correction 3, amended by everything landed since — the landed surface is authoritative: PREPARE with E-GX-IMAGE, CHECK/checked-prep for the mapped arm, CHECK-ALLOC/checked-prep-alloc for the allocated arm, gpt2-model, MODEL-DISPOSE, the refused arm-name convention). Two commits in one lane. COMMIT 1 — RELINQUISH ( GPT2TX:prep -- SAFET:census ): the prep-to-census exit the original contract omitted (S6b3 review forward-risk ii): unpacks the prep block, disposes the sealed table via WSTORE:TABLE-DISPOSE, frees the block, returns the census intact and usable; the counters prove total conversion (prep gone, census live, table gone); linearity negatives; this is what lets BIND refuse late and still hand back what the caller handed in. COMMIT 2 — BIND ( SAFET:tensor-census MDLCFG:mcfg WSTORE:residency -- GPT2TX:bind-result ) with payload ENUM bind-result 0 = bound(FIELD m gpt2-model) | rejected(FIELD c SAFET:census, FIELD code n): thin dispatch, no validation of its own — PREPARE, then per the residency arm CHECK+COMMIT-MAPPED or CHECK-ALLOC+COMMIT-ALLOCATED; a PREPARE rejection passes through; a CHECK/CHECK-ALLOC refusal (only the surfaced memory case is reachable — the identity refusal is unreachable by construction since one mcfg feeds both stages, and the header SAYS so instead of promising a foreign leg it cannot have; S6b3 review forward-risk i) converts via RELINQUISH to rejected(census, code). Fixtures per rev-4: both arms bind the same hermetic fixture and the probed weights are byte-equal ACROSS ARMS (mapped span bytes equal allocated span bytes for the same tid — vector AND Conv1D, the multi-role probe the reviews demanded); a rejected census binds successfully on a second BIND; the mapped-arm rejection fixture proves detach was never reached (mapping authority still with the census); real-artifact leg binds both arms of the 548 MB checkpoint. Mutation kills: RELINQUISH leaks the table (counter leg reds); BIND swaps the arms (cross-arm byte-equal leg reds on residency mismatch — assert the store arm through registry or dispose byte-count difference); rejected passthrough drops the census (usable-after leg reds). STOP on any checker miss or any need for new GPT2TX/WSTORE surface beyond RELINQUISH. Acceptance: the three bind suites + weight-store + maki/test.f green; both diff lints; refine-lint (no new mints expected beyond possible RELINQUISH internals — justify any); error-code-lint (codes from the remaining -5672..-5674 tail if needed). Claim: agent=s6b4 workspace=.jj-ws/habu-s6b4-bind

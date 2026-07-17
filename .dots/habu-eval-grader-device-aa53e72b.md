---
title: "Eval grader: device-launch faults must grade, not crash"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T09:25:12.698468+02:00\""
---

Found by the zedreturn sweep 2026-07-17, the first-ever on-device run of the checker-ablation block (subsystem-pkgs s4 block 3; pending-zed since mint, exits 60 SKIP off-Orin, NOT in maki/test.f so gates were unaffected). Invariant violated - the grader's own stated contract at maki/eval-device.f:108: 'candidate is a graded failure, never a grader casualty'. NOT a checker miss: the ablation's no-check arm deliberately bypasses the checker to measure its value. Root cause: GRADE-NOCHECK-CANDIDATE (eval-device.f:151) and GRADE-CANDIDATE (:141) wrap emit-fail and ptxas-fail but NOT the device launch: GRADE-DEVICE-VERDICT (:135) -> DEVICE-CORRECT? (:81) -> ED-RUN -> CUDA:RC0 throws E-CUDA (-5002) on nonzero CUresult. In eval-compare.f the no-check arm runs all 9 candidates through device; a type-buggy-but-ptxas-clean candidate (e.g. eval-compare.f:66 raw span pointer as grid ctx) does an out-of-bounds GPU read -> contained nvgpu MMU page fault (dmesg: invalid pde, virt read, fault addr 0x68c15c000, client t1_6; driver killed channel 507 and recovered) -> uncaught -5002 crashes the grader before any tally prints. Fix: wrap the device-launch verdict in catch; grade a launch fault as a distinct EVN-DEVICE-FAULT bucket (or EVN-DEVICE-WRONG - decide and document); recover/reset the CUDA context per candidate OR fork each unchecked candidate like GRADE-EMIT/GRADE-PTXAS already isolate via subprocess (prefer the fork - a faulted context is not trustworthy for the next candidate); revisit the s4 expected tallies (NC2=3/NU-WRONG=6 assumed faulters complete as device-wrong - never before produced on device); add a negative regression: a ptxas-clean-but-launch-faulting candidate must be GRADED with the run continuing (device-gated test, wire in the skipped-off-device pattern). Files: maki/eval-device.f, maki/eval-compare.f, device-gated test. Ownership: eval grader harness. Device-gated for the final proof; the catch/fork restructure is host-writable.

Claim: agent=grader workspace=.jj-ws/fable-grader (host leg only - zed owned by wave2 lane; device proof is a follow-up leg)

HOST LEG LANDED 2026-07-17 (grader lane, commit d9c4c5e9): GRADE-DEVICE-VERDICT
now fork-isolates each candidate's device launch (PROC-FORK child inits CUDA
fresh, classifies under catch, dies with ED-EXIT-GREEN/WRONG/FAULT $21/$22/$23;
parent maps exited/signaled/timeout via ED-OUTCOME>VERDICT - signal death and
timeout classify FAULT, fail-closed). New distinct bucket EVN-DEVICE-FAULT(4)
= kernel crashed, vs EVN-DEVICE-WRONG(2) = ran with bad values. eval-compare
tallies re-derived: NU-WRONG 6->5, NU-FAULT 0->1 (candidate 6, span pointer in
addressing position, is the only expected faulter; candidates 7/8 flagged
uncertain in-file). New device-gated negative regression
maki/eval-device-fault-test.f (faulter graded FAULT, grader survives, next
candidate GREEN) - deliberately NOT yet wired into maki/test.f.
DEVICE LEG REMAINING (turn-key, needs zed after the wave2 lane releases it):
run on the Orin (1) maki/eval-device-fault-test.f -> test: ok,
(2) maki/eval-compare.f -> confirm/adjust NU-WRONG=5/NU-FAULT=1/NU-GREEN=3
per evidence, (3) maki/eval-device-test.f -> unchanged 2/1/0; only after
on-device green wire eval-device-fault-test.f into maki/test.f. Known
limitation recorded: no bounded-wait on the parent - a HANGING kernel would
block the grader; if the device leg surfaces one, mint a bounded-wait dot.

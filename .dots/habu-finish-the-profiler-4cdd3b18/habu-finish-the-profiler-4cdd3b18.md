---
title: Finish the profiler and debugger tooling asks
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T00:23:54.339269+03:00"
---

Problem: the tooling half of habu-complete-queued-profiler-a5ce1ad4 (closed with the crash fix cb4fece2) is still open: profiler output names are not package-qualified so private names collide in the dump; there is no caller attribution or call chain; no stop command samples one phase without exiting; the debugger patches an unsupported breakpoint target before rejecting it. Acceptance: dump rows carry the package-qualified spelling; a caller-chain report or attribution mode; prof-off stops sampling and leaves the counters readable by prof-report; BP+ on an unsupported target fails before any code mutation, with a named refusal; each with a regression in test/gate-debug-lib.f. Files: src/habu/prof.f, the breakpoint emitters in src/habu/habu2.f (rowan-owned, handed as a commit id), test/gate-debug-lib.f, docs/debugging.md. Verify: the profiler and debugger cases through a driver over test/gate-debug-lib.f on a rebuilt engine. Depends: none. Ownership: hazel. Claim: unassigned.

Tender requested the profiler portion again in BB
`general/20260913-152852.352-tender-b3-f243`: attribute shared primitives to
their Habu caller with package-qualified identities. One caller level is enough
to separate parsing, rule captions and scoring comparisons; preserve an explicit
unknown caller when the interrupted context cannot establish one. Also stop
sampling without exiting or clearing counters so a phase can be reported later.
Current integration cabed90a still has only `prof-on` and `prof-report`; its
handler counts the interrupted PC and its dump prints the bare dictionary name.
There is no verified newer frozen pair providing these capabilities.

Reproducer: Tender ee933697, workspace
`/home/joel/Work/Tender/.jj-ws/corpus-algorithms`, `scripts/sample-score.f`;
Habu source fe51bac4 in `.jj-ws/tender-pin`, engine SHA-256
`2e12757b90c4734b13d48614c6c4842976541f82115f27ad13aa6d79492d4f9e`.
Exact invocation and private input/output paths are in the BB message. Tender's
3.9-second score yielded STR= 229, LETTER-SEARCH 153 and SPACE-SEARCH 133 samples,
but those flat counts do not identify the calling algorithm. Tender continues
its own phase counters; this feature is not blocking its work or evidence of an
engine bottleneck. Calibrate sampling overhead in a paired run; the existing
PC-index task `habu-idx-profiler-pc-45b4c841` owns the per-tick dictionary scan.

Verified on 45608866 with engine 3da80b23: the complete native-gate-debug row
passes. It pins package-qualified profiler rows and callers, an explicit unknown
caller, prof-off preserving readable counters, prof-reset and JSON. Those
profiler requests are implemented. The remaining breakpoint refusal is tracked
by child habu-refuse-breakpoints-outside-e3de0ba2; it reproduces a crash on a
real engine-text primitive and keeps the fix outside the engine protection path.
Evidence: ~/.cache/habu/profiler-verification/source-45608866/.

---
title: "Cut the checker's control flags with its records"
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T02:28:07.478381+03:00"
blocks:
  - habu-hand-the-rest-32631946
---

Problem: CHECKER-RETRACT-ROWS (src/core/checker.f:13040) cuts the effect-record store to the owner's mark and leaves the control-flag store (NORET, checker.f:15324). A dropped definition's control flags therefore apply to a later definition of its name, at both tiers: a dropped W whose body ends in `die`, then `TRUSTED: W` or `defer W`, then a certified `: T ( -- n ) W ;` prints garbage rc 67 at tier 0 (nr1, nr4) and traps `hb: w returned` rc 88 at tier 1 (nr4t1). Probes: ~/.cache/tmp/carl-forget/nr/. Measured constraints (~/.cache/tmp/carl-forget/handoff.md, Session 3): a one-cell mark cannot cut the control store exactly, because CHECKER-NAMES-ROW (checker.f:25513) appends control entries with no effect record and a definition's own control entry precedes its record (checker.f:23965); the owner fields ROWS-END-OFF and RETRACT-ROWS-OFF ($3A0, $398) are read by the previous generation's baked compiler (checker.f:20-25, CLAIM-SOURCE-OWNER :25581-25585), so a two-value mark appends fields (src/core/checker-fetch-abi.f BYTES; precedent 7bcc8270) and retires the old pair only once no host predates it; NORET-COMPACT (checker.f:26288, run by CHECKER-CAPTURE-PREPARE :26723) renumbers control offsets, so a frame mark held across a capture goes stale (included files run inside evaluate frames, src/core/include.f:1219; tests capture in-process).
Acceptance: a retract restores the control-flag store exactly as it restores the effect records, on every path that retracts the effect records (src/habu/habu1.f DECL-OWNER:RETRACT-TO's callers: both tiers' zero verdicts, the evaluate and REPL frames, the Gforth host's compile entry and text frame), including a frame that spans a capture. nr1 and nr4 give, at both tiers and on the host, the outcome they give when the dropped W never existed, and join the native suite and test/gforth/cases/. The design (mark shape, the owner-ABI migration, compaction against live marks) is settled first and recorded here.
Files: src/core/checker.f, src/core/checker-fetch-abi.f, src/core/checker-owner-abi.f, src/compiler/native/compiler.f, src/compiler/native/checker-owner.f, src/habu/habu1.f, src/habu/habu2.f, src/habu/layout.f, src/habu/data-claims.f, src/habu/stack-abi.f, src/host/gforth/codegen.fs, src/host/gforth/prims.fs, the native tests a search finds, test/gforth/cases/.
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/run.f`; two-generation build converges (the owner ABI changes across generations); `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: habu-hand-the-rest-32631946. Ownership: the files above. Worker: design on Fable, then worker-max. Claim: unassigned.

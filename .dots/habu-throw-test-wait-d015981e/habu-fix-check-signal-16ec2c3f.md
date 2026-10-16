---
title: Fix check-signal case end and watch readiness
status: closed
priority: 2
issue-type: task
created-at: "2026-10-17T00:28:15.921534+03:00"
closed-at: "2026-10-17T00:36:55.181830+03:00"
close-reason: "Landed: check-signal CASE-END runs each step under nested finally, so a CHECK-END throw no longer skips LEFT-ENDED, the PIPE-WR close or the sweep; READY-WITHIN throws E-PROC-OUTPUT on POLLERR|POLLNVAL. capture-tree-test was read and left unchanged: no step before its LEFT-ENDED can throw, and its READY-WITHIN answers feed a pipe read that fails on a broken descriptor. Proofs (macOS): row check-signal rc 0 in 33 s; forced copy check-end-throw (CHECK-END throws E-FS-CAPACITY) runs LEFT-ENDED, ends with PIPE-DONE 1, every descriptor closed, 0 scratch entries, -2106, no stray processes; the same copy on master's file skipped LEFT-ENDED and left the subject running (the gap); forced copy pollnval ends -2503 with the case end complete. Untested: Linux, full gate. Review: Astra full review APPROVE, no findings."
---

Problem: test/check-signal-test.f, landed by habu-throw-check-signal-8cb28997, has the two case-end gaps the review of habu-throw-gate-signal-10afc41c found in its sibling. (1) CASE-END runs CHECK-END, this process's PIPE-WR close and LEFT-ENDED in one protected quotation, so a CHECK-END throw (its kill, reap or log READ-ALL) skips LEFT-ENDED, the wait for what check.f left, which the CASE-END before habu-throw-check-signal-8cb28997 still ran. (2) READY-WITHIN answers the poll count without reading revents, so POLLNVAL or POLLERR on CHECK-WATCH reads as check.f ended and CHECK-REAP then blocks on a live check.f. test/capture-tree-test.f, the model both copied, was checked and needs no change: its CASE-END has no step that can throw before LEFT-ENDED, and its READY-WITHIN feeds only reads of the report pipe, where a broken descriptor fails the read and answers false.

**Files and edits**

test/check-signal-test.f: CASE-END nests finally, so the PIPE-WR close and LEFT-ENDED run after a CHECK-END throw, and the outer finalizer still closes CHECK-WATCH, FEED-WR and PIPE-RD and clears the scratch. READY-WITHIN throws E-PROC-OUTPUT on POLLERR or POLLNVAL (model: test/gate-signal-test.f READY-WITHIN?). Comments say both.

**Proof**

Row check-signal (`bin/hb --load test/check-signal-test.f`). Forced copies: CHECK-END throws where it reads the log while check.f's leftovers still hold the pipe, and LEFT-ENDED still runs and reaches end of file, with the read error winning; CHECK-ENDED-WITHIN polls a closed descriptor and throws E-PROC-OUTPUT.

**Acceptance**

A CHECK-END throw still waits for what check.f left; a broken watch descriptor throws E-PROC-OUTPUT and never reads as ended.

Files: test/check-signal-test.f. Verify: the row and forced copies under Proof. Ownership: the file above. Claim: agent=jerry workspace=.jj-ws/jerry-wait-d21.

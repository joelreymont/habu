---
title: "Count the ring's unconsumed entries at io_uring_enter"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T00:48:36.854716+03:00"
---

Problem: SUBMIT-OR-FORGET (lib/aio.f) publishes the SQE before io_uring_enter, and a refused enter leaves the published entry in the SQ ring; the next enter from any submitter asks for its own count only (ENTER-SUBMIT compares the consumed count with want), so the kernel consumes the stale entry first, that submitter is told its own entry went in while it is still in the ring, and its completion waits for whoever enters next. Latent since the loop landed (reachable only after a refused enter that took nothing); noticed in the completion-ops review. Acceptance: a submitter's enter asks for every published-but-unconsumed entry (the SQ tail minus the kernel's head, read under the facility) and judges success by the ring drained rather than by its own count; the FORGET rule stays for the ambiguous case; a regression pins the rule as far as it can be provoked, and says so where it cannot. Files: lib/aio.f, lib/aio-test.f, docs/aio.md. Verify: lib/aio-test.f, test/run.f. Depends: none. Ownership: hazel.

---
title: Decide bare hb stdin-first contract
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:14.703579+02:00"
---

Problem: bare bin/hb file.f silently interprets stdin BEFORE the file. src/habu/stdin.f lines 1-4: the installed engine reads its program from stdin unless stdin is a tty; a single NUL byte on stdin yields exit 0 with the script never run. This is a product footgun: a caller believes the file ran. Fed the 2026-07-26 gate-red incident together with the capture-inheritance seam (separate dot habu-give-captured-children-9d37d90f). Required result: decide the contract - either a file argument disables stdin-program mode, or stdin is consumed only when no file argument is present, or the current behavior is kept and documented loudly - then pin the decided behavior in test/hb-cli-contracts-test with fixtures covering file-argument-plus-stdin-bytes (script must run, or the documented alternative), empty stdin, and tty-less launch. Acceptance: the decided behavior is enforced by the CLI contract test and a mutation restoring the undecided behavior fails it. Files: src/habu/stdin.f, the CLI driver, test/hb-cli-contracts-test.f. Verify: the CLI contracts test plus a full engine rebuild fixpoint. Depends: none. Ownership: bare-invocation stdin semantics only. Claim: unassigned.

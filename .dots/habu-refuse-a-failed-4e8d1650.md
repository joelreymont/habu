---
title: Refuse a failed stdin NOSIGPIPE through the capture
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:32:49.209183+02:00"
---

Problem: src/lib/process.f:667-668 PROC-NOSIGPIPE! is FD-NOSIGPIPE! (process.f:349-350), which refuses a failed F_SETNOSIGPIPE fcntl with a plain E-PROC-OUTPUT throw. Its capture caller PROC-SETUP-STDIN-FDS (process.f:674-677, from PROC-STDIN-CAPTURE-BEGIN :1084-1086) runs after PROC-CAPTURE-BEGIN opened the stdout and stderr pipes and PROC-OPEN-PIPE the stdin pipe, so that throw leaves the three pipe pairs open and the capture state armed. Every other refusal in the capture setup goes through PROC-THROW-CAPTURE (:621-623, PROC-END-CAPTURE then throw); PROC-NONBLOCK! (:662-665), the sibling on the same descriptor, is the model. Found by the cmd-fd review (landed as 78545d76), outside that lane and unchanged by it. Acceptance: PROC-NOSIGPIPE! does the fcntl itself and refuses with E-PROC-OUTPUT PROC-THROW-CAPTURE, shaped like PROC-NONBLOCK!; FD-NOSIGPIPE! keeps its plain throw for its callers outside a capture (lsp-core.f, lsp-test-lib.f, fd-test.f, aio-test.f, content-length-test.f, gate-pool.f, process-test.f); test/check-signal-test.f:289 (CHECK-START-STDIN, its own pipe, no capture) calls FD-NOSIGPIPE! instead. No test row: F_SETNOSIGPIPE cannot fail on a fresh pipe descriptor, so the path is unreachable without fault injection. Files: src/lib/process.f, test/check-signal-test.f. Verify: hb --load tools/check.f src/lib/process.f; hb --load tools/check.f test/check-signal-test.f; hb --load src/lib/process-test.f; hb --load test/check-signal-test.f. Depends: none. Ownership: src/lib/process.f PROC-NOSIGPIPE!, test/check-signal-test.f CHECK-START-STDIN. Claim: unassigned.

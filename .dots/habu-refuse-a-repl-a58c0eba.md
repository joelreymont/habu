---
title: Refuse a REPL line longer than its buffer by name
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T05:29:03.898337+03:00"
---

Problem: src/habu/repl.f holds the line under edit in 'create LBUF 256 allot' and INSCH caps LLEN at 255, so a line typed (or piped by a fixture) past that length is silently truncated: the rule lane (2026-09-18) typed a 257-byte definition line at the REPL, the tail was dropped with no error, the word was never defined and the case died on E-UNDEFINED several interactions later. Acceptance: a line that reaches the buffer's capacity is refused by name at the REPL (the line is not accepted, the diagnostic names the limit and the length) or the buffer grows to a stated cap with the same refusal at the cap; a test drives an over-long line through a pty child and asserts the refusal; docs state the limit. Files: src/habu/repl.f, test/proc-pty.f or test/process-pty-tty-smoke.f, docs/. Verify: the test; byte fixpoint (repl.f is baked); test/run.f. Depends: none. Ownership: REPL. Claim: agent=hazel-capacity workspace=.jj-ws/hazel-capacity.

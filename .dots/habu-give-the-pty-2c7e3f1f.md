---
title: "Give the pty harness's wait a deadline"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:11:10.851728+01:00"
---

Problem: src/lib/pty-harness.f WAIT-UNTIL has no deadline, so a REPL answer a test does not expect hangs the suite (repl-open-definition sat idle 530 s). Acceptance: the wait gives up after a ceiling and answers its last answer; the pty suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-fof

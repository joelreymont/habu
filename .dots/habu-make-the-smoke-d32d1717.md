---
title: "Make the smoke test's prompt wait discriminate"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-17T13:24:25.050995+03:00\""
---

Problem: 's" habu> " EXPECT!' in test/process-pty-tty-smoke.f does not prove the REPL prompted again: the line editor redraws the prompt inside the echo of the line just typed, so the marker is satisfied by the echo, not by the prompt after the answer (pty lane, 2026-09-17). Acceptance: a prompt barrier waits for the prompt that follows a known answer marker (answer then prompt, in that order), a fixture shows the echo alone no longer satisfies it, and the suite stays green under load. Files: test/process-pty-tty-smoke.f. Verify: the fixture; ten runs. Depends: none. Ownership: process/pty tests. Claim: agent=hazel-pty-barriers workspace=.jj-ws/hazel-pty-barriers.

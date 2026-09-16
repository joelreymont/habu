---
title: "Print the runner's numeric mismatch on one line"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:08:16.454491+03:00"
---

Problem: lib/test/runner.f GT-CHECK-N mirrors lib/test/assert.f's T= shape by its own comment and still prints 'runner: expected N' then 'got N' on two lines through the newline-emitting number printer; lib/test/assert.f itself was fixed on 2026-09-16 (habu-print-a-test-54f1ae46) but the runner layer was outside that dot's files. Acceptance: GT-CHECK-N prints one line with FMT:.INT like T= does, lib/test/runner-test.f pins it, nothing parses the old form. Files: lib/test/runner.f, lib/test/runner-test.f. Verify: the runner test through bin/hb (lib/test/ is baked; rebuild to see it in children). Depends: none. Ownership: hazel line. Claim: unassigned.

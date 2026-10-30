---
title: Run tools/check.f on the Gforth recovery engine
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T11:50:32.970643+02:00"
---

Problem: on spark the Gforth-bootstrapped engine (tools/bootstrap.sh rc 0 in 141 s) exits 70 with no output on bin/hb --load tools/check.f -- src/lib/fs.f, in master's tree and in the nonblock lane's alike (2026-10-30); test/check-signal-test.f therefore never sees check.f's report there and ends rc 67 after its wait. Acceptance: check.f on the recovery engine checks a file and reports, or its refusal names the cause on stderr. Files: tools/check.f, tools/check-core.f, docs/bootstrap.md recovery route. Verify: on spark after tools/bootstrap.sh, bin/hb --load tools/check.f -- src/lib/fs.f rc 0 and bin/hb --load test/check-signal-test.f rc 0. Depends: none. Ownership: tools/check.f under the recovery engine. Claim: unassigned.

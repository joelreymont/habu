---
title: "Go on past a nested file's stop under --all-errors"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:01:16.076254+01:00"
---

Problem: under --all-errors an unclosed string, unclosed definition or loader form in a required file ends the whole composition (src/habu/verify-source.f QUIET-UNTERM), so the requiring file's later records are lost (t1.f: nosuch0, require b.f, nosuch2; b.f: nosuchb, an unclosed string: nosuch2 is missing); master stops earlier. Acceptance: the require whose file stopped is rejected and the scan goes on at the requiring file's next statement; tools/check-all-errors-core.f header says so. Depends: none. Claim: unassigned.

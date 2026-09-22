---
title: "native-checker-prefix.f is red standalone: its WHITEBOX row needs the gate's engine"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T07:56:18.188875+03:00"
---

Problem (measured by the tier-prepend lane on 5b17d667, at both tiers): bin/hb --load test/compiler/native-checker-prefix.f ends 'test: failures' rc 1 because a WHITEBOX-SUITE row's child needs the whitebox engine that only the gate (test/gate-stdlib-lib.f SUITE-WB-RUN) provides; the gate row is green. Acceptance: standalone the file either runs the row against an engine it names or skips it with one line saying the gate supplies the whitebox engine; the gate row unchanged; docs/gate.md's suite section says which rows are gate-only if any remain. Files: test/compiler/native-checker-prefix.f, docs/gate.md. Depends: none. Ownership: hazel (gate/test load path). Claim: unassigned.

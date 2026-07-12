---
title: "V2 R3: define target identity owner"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:39:30.202491+02:00"
---

Problem: CAD-KIND:target-id is declared but no semantic target descriptor or owner API exists; current report/schedule paths use display strings, so habu-v2-r3-type-2f60c17c cannot migrate an identity. Fix: add package TARGET in maki/target/target.f with immutable target descriptors, canonical target facts/digest, validated private allocation/refinement to CAD-KIND:target-id, typed lookup/projections, and no public raw n conversions; preserve separate human labels. Integrate the typed target into maki/sched-key.f without changing canonical rendered keys. Acceptance: target/toolchain and target/artifact swaps reject with qualified types; malformed/unknown descriptors fail closed; same canonical facts produce the same identity; distinct capability facts do not alias; round-trip preserves family; every private refinement has a focused test and TRUSTED.md row. Files: maki/target/target.f, target-test.f, maki/sched-key.f/test, maki/test.f, FILEMAP.md, TRUSTED.md, docs/model-cad.md. Verify: focused target/sched-key tests, typed-local diff lint, trust-lint, maki/test.f, host-lint, filemap-lint.

---
title: Repair check-all-errors and mirror fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:56:11.575293+02:00"
---

Full context: two test files are red independent of any current lane, reproduced against an unmodified base: bin/hb --load tools/check-all-errors-test.f exits 1 (test: failures; the package-caller-export case expects 0 and 70 but gets 7136 E-PKG-CONTEXT), and bin/hb --load tools/bootstrap-mirror-lint-test.f exits 1 with 35 findings. The first shares the checker package-neutral-replay root tracked by habu-neutralize-checker-pkg-b9a250c8; the second is the deliberate ADT-in-recovery-corpus tripwire owned by habu-bootstrap-mirror-pass-f1714953, whose finding count has grown as the compiler campaign added ADT declarations under src/compiler. Root-cause each against its owning dot and either fix the tool or record the missing capability. A red fixture reachable from a standard load is a stop-the-line condition, not background noise.

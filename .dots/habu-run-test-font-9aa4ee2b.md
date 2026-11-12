---
title: Run test/font-native.f in the suite
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:39:20.011816+02:00"
---

Problem: no SUITE in test/gate-*.f lists test/font-native.f or test/font-image-subject.f, so nothing runs FONT's tests; src/lib/font.f names them as its tests. Acceptance: the suite runs them where HarfBuzz is installed, or the file's header says how they run. Depends: none. Claim: unassigned.

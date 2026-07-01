---
title: Split AOT closure checker from maker entry
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T11:16:11.516393+02:00"
---

Problem: Mac hot direct suite now has slowest-test=native hb-build AOT negative gate phase at ~15.5s. test/gate-aot-negative-lib.f proves only closure-limit JSON but must run HBB-BUILD-MAKER plus HBB-RUN-MAKER-CMD because src/habu/aot.f fuses closure analysis with GO/source execution and image emission. Fix: split src/habu/aot.f into a checked reusable closure/report library plus thin maker entry, expose a resident semantic closure-limit API for GAN-CLOSURE-LIMIT, and keep at most one hb-build CLI/maker boundary sentinel for real maker behavior. Acceptance: AOT negative <=5s Mac hot full suite, closure-limit JSON fields still asserted, maker-run counter does not come from semantic closure-limit test, full suite green.

---
title: Find what a killed build leaves in the build cache
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.736671+02:00"
---

Problem: a build killed partway left HABU_BUILD_CACHE in a state where every later build of an entry named maki.f failed 'hb: uncaught throw code 7122' (E-LAYOUT-BOUNDS); the same bytes built under another entry name or with a fresh cache. src/lib/build-cache.f's header says entries are keyed and published by rename and that a killed build's work directory is reaped (WORK-OPEN :886), so the leftover that a later build trusts is outside that route or keyed by the entry name. Cause unknown; the damaged cache is gone. Acceptance: reproduce by killing an image build at successive stages under one HABU_BUILD_CACHE, find the entry a later build reuses, and make every cache write atomic or validated so a killed build cannot make a later one fail; a regression row that kills a build and rebuilds. Files: src/lib/build-cache.f, tools/build-support.f, tools/build-out.f. Verify: the kill-and-rebuild row. Depends: none. Ownership: build cache. Claim: unassigned.

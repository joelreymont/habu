---
title: Write the names sidecar for stripped applications
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T01:17:31.897948+03:00"
---

Problem: tools/native-build-core.f writes <image>.names for the engine (e74437cf) but tools/hb-build.f, which shakes an application out of the image and strips it, writes no map, so a debugger or imgdump over a shipped application has code it cannot name. WRITE-NAMES in native-build-core.f is factored to be lifted. Acceptance: hb-build writes <app>.names in the same format (version line, columns line, one row per record kept or stripped) for every application it emits, from the closure it computed, with a fixture in test/ that builds a small application and checks a stripped word appears in the map and not in the image's dictionary; the two writers share one implementation. Files: tools/hb-build.f, tools/hb-build-lib.f, tools/native-build-core.f (shared writer), test/. Verify: the fixture; test/hb-build-test.f. Depends: none. Ownership: build tools. Claim: unassigned.

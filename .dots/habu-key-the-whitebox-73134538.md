---
title: Key the whitebox host on its source closure
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T07:48:00.930277+03:00"
---

Problem: test/whitebox-engine.f keys the whitebox host's build-cache entry on the building engine's own bytes ("whitebox-engine-v1" + bin/hb), not on the source closure the image is built from, because tools/native-build.f's closure blows the shared walker's EC-MAX (E-FS-CAPACITY) that test/cold-engine.f keys with; so the whitebox host tracks the installed binary, and a tree whose engine sources changed without a reinstall gets a stale whitebox host while the rest of the gate tests the installed binary (whitebox lane, 2026-09-17). Acceptance: the closure walker's cap grows to hold tools/native-build.f's closure (a named constant with its cost), test/whitebox-engine.f keys on that closure like test/cold-engine.f, and a source edit under src/ invalidates the cached whitebox host without touching bin/hb; a fixture edits one prefix file in a copy and shows the key change. Files: lib/event-closure or tools/event-closure-lib.f (EC-MAX), test/whitebox-engine.f, test/cold-engine.f. Verify: the fixture; test/run.f. Depends: none. Ownership: gate harness. Claim: unassigned.

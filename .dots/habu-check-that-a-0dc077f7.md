---
title: Check that a manifested file in a keyed closure loads nothing
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.057657+03:00"
---

Problem: src/habu/driver-io.f and src/core/include.f sit inside the whitebox host's keyed closure while listed in tools/dynamic-tail-manifest.f, so the event log is only a lower bound for them and the key is complete only because both load nothing; nothing enforces that, and tools/source-discovery-test.f SDT-TEST-MANIFEST-INCLUDE pins it for include.f alone (whitebox-key lane, 2026-09-17). Acceptance: a check in the closure walker or the key derivation refuses a manifested file reachable from a keyed closure that records any loader event, with a fixture for each of the two files and a negative that adds a require to a copy. Files: tools/event-closure-lib.f or test/whitebox-engine.f, tools/source-discovery-test.f. Verify: the fixtures; test/whitebox-engine-key-test.f. Depends: none. Ownership: gate harness. Claim: unassigned.

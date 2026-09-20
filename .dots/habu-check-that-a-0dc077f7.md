---
title: Check that a manifested file in a keyed closure loads nothing
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.057657+03:00"
---

Problem: src/habu/driver-io.f and src/core/include.f sit inside the whitebox host's keyed closure while listed in tools/dynamic-tail-manifest.f, so the event log is only a lower bound for them and the key is complete only because both load nothing; nothing enforces that, and tools/source-discovery-test.f SDT-TEST-MANIFEST-INCLUDE pins it for include.f alone (whitebox-key lane, 2026-09-17). Acceptance: a check in the closure walker or the key derivation refuses a manifested file reachable from a keyed closure that records any loader event, with a fixture for each of the two files and a negative that adds a require to a copy. Files: tools/event-closure-lib.f or test/whitebox-engine.f, tools/source-discovery-test.f. Verify: the fixtures; test/whitebox-engine-key-test.f. Depends: none. Ownership: gate harness. Claim: unassigned.

Claim: alder, .jj-ws/alder-manifest-key on 42f3e819. Check manifested members
when deriving whitebox/cold keys, without restricting ordinary closure walks.
Copy the two real boundary files into a private invocation root and prove that
adding a literal require there refuses the key; leave the checkout untouched.

Implemented EC:CHECK-KEYABLE over the current closure, using each member's
stored resolution root. Whitebox/cold key derivation checks before opening the
fold. Ordinary EC:BUILD and LOAD-ORDER retain their existing behavior.

Proof: whitebox-engine-key-test copies the real driver-io.f and include.f into
a private invocation root. Each child first proves DTM recognition and zero
loader events; the clean file keys successfully. Appending require lib/errors.f
records exactly one event and must refuse with E-DISC-DYNAMIC. Both negatives
failed before (child assert 3: expected -4101 got 0) and pass after. No checkout
source is modified by the fixture. Independent Astra xhigh review: no blockers.
All 20 focused registry rows passed: event-closure, source-discovery,
object-image-writer, source-root, hb-build-fixtures, whitebox-engine-key,
whitebox-engine, cold-argv-separator, aot-named-cells-image and all eleven
aot-wid modes. Evidence: /tmp/alder-manifest-key. The build-fixpoint-fixtures
row invokes prohibited install --force and is deferred to Hazel's serial gate;
no separate full gate or engine-source edits.

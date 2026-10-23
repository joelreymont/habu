---
title: Install slot rows from file objects (L2a)
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T23:12:45.492683+03:00"
blocks:
  - habu-stop-file-obj-89336a60
---

WIP rzqvmwot on wpzouxtn, workspace .jj-ws.noindex/carl-l2a; design /Users/joel/.cache/tmp/carl-file/design-l2.md, brief brief-l2a.md, handoff l2a-handoff.md (Hand-off 3 findings). Blocker A answered: cold path binds a does>-patched record twice (RETIRE-REUSED NFR:665-672 rebinds on the does> re-stamp) and uses up a name occurrence; payload keys then miss warm (-8597). The fix goes to the hit lane (habu-stop-file-obj-89336a60). B: warm != cold product with CHECK-RESOLVE LOCAL reverted (TFAIL 7 12 18); probe-c cold-vs-cold also differs, but used separate cache dirs; rerun the slots fixture after A's fix before any B lane. C (pool order): not run. Probes /Users/joel/.cache/tmp/carl-l2a/probe/. Fixture test/native-file-slots-e2e.f still carries <<DEBUG/<<RUN prints to remove.

Resumed 2026-10-24: rzqvmwot rebased onto stack tip zoqvzolv (A fixed there by 89336a60: RETIRE-REUSED keeps a re-stamped row key). Worker in .jj-ws.noindex/carl-l2a; handoff l2a-handoff.md.

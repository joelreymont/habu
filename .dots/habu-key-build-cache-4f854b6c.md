---
title: Key build-cache artifacts on tree-relative paths
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.055509+03:00"
---

Problem: lib/content-key.f FILE+ folds the absolute pathname (CK-FILE-TAG a u) into the preimage, so every workspace of the same tree keys a different whitebox and cold host and pays the 70 s build again; now that the whitebox key folds the builder's 159-file closure (7967a5b8) the cost is paid per workspace (whitebox-key lane, 2026-09-17). Acceptance: the key folds tree-relative paths plus content, two workspaces at the same tree content resolve the same artifact path in ~/.cache/habu-build, an edited file still moves the key, and the existing key fixtures pass. Files: lib/content-key.f, test/cold-engine.f, test/whitebox-engine.f, test/whitebox-engine-key-test.f. Verify: the fixtures; two workspaces at one commit share the host. Depends: none. Ownership: gate harness. Claim: unassigned.

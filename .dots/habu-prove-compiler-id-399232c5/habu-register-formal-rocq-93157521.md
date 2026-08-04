---
title: Register formal Rocq build
status: closed
priority: 1
issue-type: task
created-at: "2026-07-28T09:10:35.870780+02:00"
closed-at: "2026-08-04T20:24:43.807344+02:00"
close-reason: "delivered and moot: formal/_CoqProject and formal/Makefile exist and build the four Rocq files, with the assumption manifest enforced by test/compiler/ir-id-proof.f; the FILEMAP.md half died with commit 85a9646fd, which deleted FILEMAP.md and filemap-lint. No dot depends on it."
blocks:
  - habu-bind-compiler-id-596761f1
---

Claim: agent=formal_build workspace=.jj-ws/habu-register-formal-rocq-93157521

Full context: the four Rocq files under formal/Common (Ids.v, IdAllocator.v, IdLaws.v, IdAllocatorLaws.v) compile clean on Rocq 9.2 but have no build project and no file-map entry. Two gaps: (1) there is no formal/_RocqProject (or equivalent) declaring '-Q formal Habu' plus the four files in dependency order, so every dot's focused command is a hand-typed four-command sequence and habu-bind-compiler-id-596761f1 has no single 'compile all four Rocq files' entry point; (2) FILEMAP.md documents test/compiler/ir-id.f and test/compiler/ir-id-concurrency.f but has no entry for formal/ at all, so filemap-lint has no record of the proof artifacts. Acceptance: one committed build project builds all four files in dependency order from a clean state, reporting no Admitted and exactly the two expected assumptions (host_atomic_cas, atomic_cas_linearizable); FILEMAP.md describes formal/ and each .v file; filemap-lint passes. Keep the build declaration data-only.

Update 2026-07-28 (orchestrator): the build project half is DONE and verified —
formal/_CoqProject and formal/Makefile exist and `make -C formal` builds all
files from the current tree cleanly (exit 0), with the assumption manifest
enforced by the identity parity gate (test/compiler/ir-id-proof.f). The
FILEMAP.md half is moot: FILEMAP is being removed per the user's direction.
Nothing remains to implement; the dot stays active only until the merged work
is pushed and verified at the remote per closure procedure. The claimed
workspace .jj-ws/habu-register-formal-rocq-93157521 no longer exists; the
claim is released.

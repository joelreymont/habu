---
title: "M2: safe filesystem primitives"
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:03:36.459884+02:00"
blocks:
  - habu-epic-one-structure-04f9804f
---

Start only after M1 is closed on green master. Own the checked final-component no-follow streaming reader and alias-safe same-directory atomic replace, including target constants, nominal effects, structured primary-plus-close/write/cleanup outcomes, native/recovery parity, hostile filesystem fixtures, manifests/docs, and exact focused/full gates. Existing owners: habu-fs-checked-no-7b20610f and habu-fs-make-atomic-61537711. Finish by fast-forwarding a green FS milestone to master so later compiler/scanner work consumes one stable checked FS API.

RECOVERY POINTER 2026-07-18 (workspace forensic sweep): this milestone's implementation exists only in held workspaces, never on master: lib/fs-atomic.f (package FS-ATOMIC, ~605 lines, alias-safe same-directory atomic replacement), lib/fs-stream.f (package FS, checked no-follow regular-file streaming), tools/fs-primitive-parity-test.f, directory-relative mkdir. Fullest tip: workspace sol-safe-change at 7218b9ea; siblings habu-change-file-integration 92593f80, habu-fs-checked-no-7b20610f fa38d9e2, habu-nofollow-repair d2a5fe4b, sol-mkdirat e4afb736, sol-primitive-proof fc113005 are earlier attempts on the same lineage. Recover from the fullest tip; retire siblings after it lands or is adjudicated. Do not delete these workspaces before then.

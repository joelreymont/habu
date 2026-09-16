---
title: Bake prefix source names tree-relative
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.731250+03:00"
---

Problem: the engine bakes the absolute paths of the build tree (for example /tmp/hazel-G6/tree/src/core/top-row.f, seen in the prefix source-name table); two builds of one revision from different directories differ in 111 bytes, all inside those paths, so the binary is not reproducible across build directories and a fixpoint check must build both engines from the same path. Acceptance: source names recorded relative to the tree root (or omitted where nothing reads them), diagnostics still name the file, two builds of one revision from different directories are byte-identical. Files: src/habu/habu2.f (source tape and include names), tools/native-build.f. Verify: build the same revision from two directories with the same host and cmp. Depends: none. Ownership: hazel line. Claim: unassigned.

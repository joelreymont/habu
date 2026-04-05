---
title: Remove borrowed call slices
status: open
priority: 1
issue-type: task
created-at: "2026-04-05T11:37:31.445392+02:00"
blocks:
  - habu-split-load-apis-e4c62eb7
---

Problem: PLAN.md 1.7b still needs the compiler call-node surface cut over from borrowed slices to owned/canonical argument storage so long loads and macro-heavy compilation do not retain invalid slice lifetimes. Acceptance: compiler-generated call nodes keep stable owned argument storage only; no borrowed-slice IR remains on compiler-generated call paths. Files: PLAN.md:527-541, src/compiler/ir.zig, src/compiler/compile.zig, src/bytecode/emit.zig. Verify: focused compiler regression plus long-load smoke on canonical Maxima modules.

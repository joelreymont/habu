---
title: "Gate docs/repair-diagnostics.md's repair-class list"
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T05:16:49.917870+03:00"
---

Problem: docs/repair-diagnostics.md's repair-class list has drifted across several landings: it lacks declare_pointer_cell (habu-refuse-a-ptr-5ad2734e), fix_opaque_execute, fix_host_region, fix_host_extent, fix_stale_generation and move_collective_to_block_uniform_control (rule lane, 2026-09-18), and tools/repair-schema-doc-test.f does not gate it. Acceptance: the doc lists every repair class src/core/render.f emits, generated from or checked against the render table by tools/repair-schema-doc-test.f so a new class without a doc row is a red test; the six missing rows added with one line each. Files: docs/repair-diagnostics.md, tools/repair-schema-doc-test.f, src/core/render.f. Verify: tools/repair-schema-doc-test.f; test/run.f. Depends: none. Ownership: diagnostics docs. Claim: unassigned.

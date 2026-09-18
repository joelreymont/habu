---
title: Release the old lint slab when its buffer grows
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T19:58:28.961075+03:00"
---

Problem: tools/lint/text.f:138-151 allocates a replacement mapping and overwrites the slab pointer/capacity without releasing the old mapping. Reproduced through public LINT-SLAB:LOAD: load lib/prelude.f into a zero-initialized caller slab (capacity 65536), save its TEXT pointer, then load src/core/checker.f (capacity 720896). MAPPED:LIVE? from lib/test/mapped.f still returns true for the old pointer. Successively larger files accumulate abandoned mappings in long-running lint processes. Acceptance: allocate/install/release the replaced span while preserving allocation-failure behavior; exercise multiple growth steps and verify the old mappings are absent with MAPPED:LIVE?, while the current text is correct and smaller reloads reuse capacity. Do not infer release from an RSS threshold. Files: tools/lint/text.f and focused lint text tests. Verify: focused text/lint suites through bin/hb. Related but distinct from habu-declare-the-tools-a6847327, which owns record representation. Ownership: lint storage. Claim: unassigned.

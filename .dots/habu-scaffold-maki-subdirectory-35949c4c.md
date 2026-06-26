---
title: Scaffold maki/ subdirectory + habu seam
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.334397+02:00"
---

D.4. Create maki/ as an application-Forth subdir on bin/hb with a strict one-way dep on habu. Includes: the dir, a README pointing to PLAN.md, maki/STATUS.md (its own self-check namespace), the maki-* dot naming convention, and the DOCUMENTED bin/hb --load prelude (the habu lib order from docs/bootstrap.md + lib/ptx.f + the PTX layer, then maki files). Maki stays CHECKED/typed Habu (CHECKED: + real effects); the fence excludes it from TRUSTED.md/fixpoint/native-gate only. NO blocker - starts immediately.
- Files: maki/README.md, maki/STATUS.md, maki/ tree.
- Verify: maki source --loads and checks clean through its documented prelude; fence dots (dependency lint habu-add-maki-one-faa453a4 + host-lint/stale-status skips) keep the gate green with maki present.
- Dep: none technically; coordinate with the 3 fence dots.

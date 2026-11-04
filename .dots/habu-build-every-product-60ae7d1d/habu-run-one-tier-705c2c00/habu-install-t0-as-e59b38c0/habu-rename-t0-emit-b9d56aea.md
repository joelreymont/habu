---
title: "Rename T0-EMIT's PROVIDED buffer off the loader word"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T03:22:46.720291+02:00"
closed-at: "2026-11-04T11:48:48.599321+02:00"
close-reason: "Census 2026-11-04, done: PROVIDED buffer gone: t0-emit.f now has EMIT-PROVIDED? reading INSTALLED (t0-emit.f:516); verified in tree."
---

src/habu/t0-emit.f:158 `ARCHES TYPED-BUFFER PROVIDED bool` spells the loader word `provided`, so `bin/hb --load tools/check.f -- test/t0-install.f` (any file loading T0) refuses E-LOADER-FORM at t0-emit.f:158 (tools/source-discovery.f: a definer taking a loader word's spelling rejects fail-closed). --verify-only is unaffected. Introduced in vxokuxslptpu "Give T0 one emitter seam"; fix there (rename the buffer, uses at :504 :510) once no lane worker is active on the stack, prove with the full check over test/t0-install.f. Workspace: jerry-a3d (lead).

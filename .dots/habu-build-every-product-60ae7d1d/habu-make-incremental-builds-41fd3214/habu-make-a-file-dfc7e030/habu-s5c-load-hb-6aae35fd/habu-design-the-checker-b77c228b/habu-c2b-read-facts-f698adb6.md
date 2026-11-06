---
title: "C2b: read facts tags and the fresh-atom ordinal"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.556621+02:00"
blocks:
  - habu-c2a-name-the-16c6a0ff
---

Problem: design r2 D2: facts text carries tags, each read beside its reader (noret, throws and intact masks, zero-true/zero-false, core-op, intrinsic, parses, parse-imm, creates, names, generates, renders, nominal, defer, trusted-only, top-only, noscope, offset/cell/null/raw), and ATOM-REND's fresh-atom ordinal; source signatures already spell fresh-region-b as a name (docs/forth-card.md:191-192, docs/forth.md:1787), so only the facts-text parser reads a trailing -<letter> as the ordinal. Acceptance: the renderer writes and the facts parser reads every D2 tag and the ordinal; source signatures parse as today. Files: src/core/render.f, src/core/checker.f. Verify: the fid2 probe (fresh-atom and tag differences become same); bin/hb --load test/run.f -- engine checker-soundness. Depends: C2a. Ownership: those files. Claim: unassigned.

---
title: "maki duplication: op-kind ladders, launch bodies, CAD-NUM reopens"
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:26.009671+02:00"
---

Problem: exhaustive MATCH opkind tables over ~40 variants duplicated in maki/lower/ew.f:115-132,311-337, mm.f:141-158,159-176 and red.f (six tables to edit per new op; docs/forth.md:700-703 says row data; op-registry.f already has OPR-COMPLETE?/CLASS-*); seven 'package CAD-NUM public : X-IC>N' reopens (ew.f:59, mm.f:84, red.f:76, launch.f:65, golden.f:38, lower/golden.f:48, cad.f:121) while the same files seal SAFET with prot-wid-add to forbid exactly that; maki/gpu.f:58-74 child-process PTX emit with an 11-file hand-listed --load where in-process PTX-CAPTURE-ON/OFF exists (gpt2-model.f:857-869). Acceptance: one registry column per lowering class; the reopens gone with the CAD-NUM dot; the child-process emit deleted. Files: maki/lower/*.f, maki/op-registry.f, maki/gpu.f. Verify: maki/test.f. Depends: the CAD-NUM shim dot. Ownership: maki. Claim: unassigned.

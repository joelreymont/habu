---
title: "C2b2: read a word's control facts from facts text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:29:53.876111+02:00"
blocks:
  - habu-c2b-read-facts-f698adb6
---

Problem: design r2 D2 (/tmp/jerry-big/design-facts-r2.md): after C2b the facts text carries an effect's variables, but not the facts about the word a caller's check reads, each kept today outside the effect: noret (CTL-DEAD, DEAD-CUR?), throws and its two intact masks (CTL-THROW, XFER-PACK bits 20-59; CALL-THROW-EDGE, BTICK-EDGE), zero-true and zero-false (ZERO-USE-TEST), core-op (TOK-CORE-OP?), intrinsic by its INTRINSIC-* name (TOK-INTRINSIC, SYM-INTRINSIC), parses and the count parses: declares (TOP-RUNS, PARSES-TARGET), parse-imm and its PIMM count (REJECT-IMMEDIATE), creates with the created effect (NORET.CREATES; DO-TOK), names (BIND-TOK, verify-source.f TOP-NAMED), generates (DECLARED-RENDER?), renders and nominal (CHECKER-VERIFY-RENDERS), defer (IS-TOK, TOP-CONTROL), trusted-only (PRIM-TRUSTED-SYM?) and top-only (PE-TAKES?). Acceptance: REND-FACTS writes a tag for each fact a symbol holds and SIG-TAGS reads each back into a result beside the parsed effect; the fid2 probe, extended to compare those facts, finds every live symbol's facts read back equal; the operand spelling of the masks and counts is decided in the brief. Files: src/core/render.f, src/core/checker.f. Verify: the probe; bin/hb --load test/run.f -- engine checker-soundness. Depends: C2b. Ownership: those files. Claim: unassigned.

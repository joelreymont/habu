---
title: checker certifies bodies the engine cannot load
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:46:49.262680+02:00"
---

Problem: the checker holds PRIM: rows for spellings that have no dictionary record - 'variable' and 'constant' are interpret-state define keywords only (src/habu/habu2.f:6727-6728 EM-INTERPRET-DEFINE-KEYWORDS), so ': MKV variable ;' certifies through tools/check.f's source preverify (as '( -- )' after the 2026-08-23 row fix) and then dies 'E-UNDEFINED: variable' rc 70 at load, with no checker packet - a file the verifier passes and the engine cannot compile (measured by the axiom-rows lane; a control ': MKZ nosuchword ;' does yield a checker packet). The rows exist because the check hook seeds 'NAME constant' / 'NAME create' bodies through them at definition time (C-DEFHOOK), so they cannot be deleted. Acceptance: Checker-Miss RCA; the verifier distinguishes a definer-hook seed token from a user body token (the seeded body is the engine's, not source) and refuses a user body that names an interpret-only keyword with a named code at check time, so preverify and load agree; fixtures: ': MKV variable ;' refused by check.f with a packet, the seeded create/constant hook bodies still certified, the recovery gate green. Files: src/core/checker.f (DEFINER-TOK/DO-TOK), tools/check-core.f, test/create-axiom-test.f. Verify: the fixtures; maki; the recovery probe. Depends: habu-var-and-const-0af7da85 (landing). Ownership: checker. Claim: unassigned.

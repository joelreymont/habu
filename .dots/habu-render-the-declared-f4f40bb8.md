---
title: Render the declared row as written on a borrowed-row diagnostic
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:49:57.681102+03:00"
---

Problem: on a diagnostic raised after a step bound the definition's declared base row (an input underflow, habu-name-an-input-45ee675e, measured 2026-09-18 by that lane on release e7b1e2dc and on the fixed engine alike), the JSON field declared_effect renders the BOUND row: ': G ( -- n ) CONSUME 0 ;' reports declared_effect 'n -- n n' for a definition written '( -- n )', while declared_effect_source correctly says '-- n'; a repair loop reading declared_effect sees an input row the source never declared, so its repair targets a signature that does not exist. Pre-existing, not caused by the underflow work. Acceptance: declared_effect renders the declared rows as the signature parser produced them (a snapshot taken before the body binds anything, or rendering through the unbound copy the seal keeps), so it equals declared_effect_source modulo formatting on every diagnostic; a case in test/compiler/input-underflow-refusals.f asserting declared_effect for the bare underflow, and one in the golden diagnostics if any golden file changes (report, do not update, before deciding). Files: src/core/render.f, src/core/checker.f (where the declared rows are captured), test/compiler/input-underflow-refusals.f, docs/repair-diagnostics.md. Verify: the fixtures; the diagnostic suites; three generations with cmp (checker.f is baked); test/run.f. Depends: habu-name-an-input-45ee675e landing. Ownership: checker diagnostics. Claim: unassigned.

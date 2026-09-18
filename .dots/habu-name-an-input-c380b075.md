---
title: Name an input underflow through >r, 2>r and execute
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:12:44.675587+03:00"
---

Problem: the underflow lane (habu-name-an-input-45ee675e) names an input underflow at the consuming token only for steps that go through CHECKER-STEP's UNIFY-IN; >r, 2>r and a quotation's execute consume through a bare UNIFY with no capture (src/core/checker.f, the return-stack and quotation arms), so ': G ( -- ) >r ;' still reports the boundary backstop CHECK-NO-BORROW's bare 'at '>r'' with no reason after that lane lands. Acceptance: those three arms measure the borrow before their unify the way CHECKER-STEP does (STEP-BORROWS? or its equivalent for the arm's row) and refuse with E-INPUT-UNDERFLOW, repair class supply_missing_input, the reason carrying both counts, pinned at the consuming token; cases added to test/compiler/input-underflow-refusals.f for >r, 2>r and execute plus a certifying control; CHECK-NO-BORROW stays as the backstop. Files: src/core/checker.f, test/compiler/input-underflow-refusals.f. Verify: the fixture; three generations with cmp (checker.f is baked); test/run.f. Depends: habu-name-an-input-45ee675e. Ownership: checker. Claim: unassigned.

---
title: "Test premises: an explicit form or real values"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:44.327013+02:00"
---

Phase 7 of 4fd12d60: test/ holds 826 TRUSTED: sites; ~395 discharge via the visibility capability (fab55650) and most others via phases 2-5. The residue is deliberate fixture lies (~98 non-empty casts + ~59 phantom test sites, engine-suite.f:414 'here and nowhere else'). RULING (overridable): production TRUSTED: dies with no test exception; tests that need a stated premise get a TEST-SCOPE axiom form (FIXTURE:), loadable ONLY under the test loader path - enforced by the loader, not a lint - and the census must show it is a rename confined to test/, never a production hatch. If a fixture can use real values instead, it must. Blocks the final deletion.

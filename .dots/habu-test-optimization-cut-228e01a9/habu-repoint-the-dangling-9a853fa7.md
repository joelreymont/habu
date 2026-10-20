---
title: Repoint the dangling Retirement pointers
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T17:42:16.097281+03:00"
closed-at: "2026-10-14T19:33:10.235898+03:00"
close-reason: "Implemented in 54e601b2: retirement pointers were mapped to live owners or removed with retired boundaries; the pointer census found 59 valid owners and no dangling ids, with affected suites and byte-identical engine checked."
---

Found by the stage2size fold (473): 28 of 31 'Retirement: <dot>' comments on TRUST rows and unchecked boundaries in src/ src/lib/ tools/ test/ name dots that no longer exist anywhere under .dots/: habu-builder-trust-rows-c5d41af6 (20 sites, e.g. src/habu/stage2.f:19), habu-checker-self-typing-9ff8ba86 (2), habu-primitive-effect-axiom-1119f176 (2), habu-raw-self-path-4514ffd3 (2), habu-multishot-quotations-typed-8832cace (1), cap:checker-hook-identity (1). History: they were dropped by d1a62c50 'Rebuild the tracker around seven campaigns'. Only habu-attr-and-remove-2b13e978 (3 sites) still resolves. A retirement pointer that resolves to nothing leaves each unchecked boundary with no owner. Acceptance: every Retirement pointer names a live dot that owns the retirement (the campaign dot that absorbed the work, or a new one), or the boundary is retired; rg 'Retirement: ' over src lib tools test finds no dangling id.

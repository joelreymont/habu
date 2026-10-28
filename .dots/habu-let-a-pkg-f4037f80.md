---
title: "Let a package's NEW resolve under using"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T00:32:54.616220+02:00"
---

Problem (phil, found building TASK:NEW): bare NEW under 'using TASK' is refused E-UNDEFINED: NEW (exit 70), not E-USING-SHADOW-GLOBAL as docs/forth-card.md describes; a throwaway package with a public NEW gives the same on the installed engine. Inferred cause: a global word NEW (src/core/checker.f:3595) with no checker signature hides any package's public NEW under using. Qualified TASK:NEW works; queue.f (the only using TASK user) has no bare NEW. Acceptance: a reduced rejected/accepted program pair through the real load path shows which rule holds (the card's E-USING-SHADOW-GLOBAL, or resolution to the package word); the checker's global NEW either gets a signature, goes private to the checker, or is renamed, so that resolution follows the card; card and forth.md 'Rules learned by refusal' agree with the measured behaviour. Files: src/core/checker.f, docs/forth-card.md. Depends: none. Claim: unassigned.

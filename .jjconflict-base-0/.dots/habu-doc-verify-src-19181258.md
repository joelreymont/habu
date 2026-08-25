---
title: Document VERIFY SOURCE-BUF package inheritance
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:32:14.806701+02:00"
---

Full context: VERIFY:SOURCE-BUF (as distinct from SOURCE-BUF-IN-SCOPE) opens CHECKER-CANDIDATE-SCOPE-START, which INHERITS the caller's package. That is correct for candidate probes of the caller's own code, but any consumer using it to check STANDALONE source from a package-owned caller has exactly the bug that habu-neutralize-checker-pkg-b9a250c8 just fixed for the check-core replay sites. Inventory its callers, migrate the ones replaying standalone source to a neutral scope, and document the boundary at the definition so the distinction is not rediscovered by a future failure. Acceptance: each caller is classified in the report, and the definition carries a comment naming which of the two contracts it provides.

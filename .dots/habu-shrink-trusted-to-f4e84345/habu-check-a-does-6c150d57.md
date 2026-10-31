---
title: "Check a DOES> body's declared fresh inputs as its callers see them"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T15:26:29.176777+02:00"
---

Problem: src/core/checker.f CHECK-DOES-RUN builds a DOES> body's rows straight from its signature, so a declared fresh-* input stays a template atom (negative key) in the body, and REGION-ATOM? / ATOM-OK? refuse it, the same defect habu-pass-a-fresh-bb992e31 fixes for colon bodies (lane frgn worker found it; it removed an untested call it had added there). Acceptance: a DOES> body whose signature declares a fresh-region input passes it to a region-generic word and returns it, as an accepted row failing on the master engine; the refusal rows of rigid-region keep their reasons. Files: src/core/checker.f, test/rigid-region-suite.f. Verify: engine build; rigid-region row. Depends: none (habu-pass-a-fresh-bb992e31 landed; reuse its SIG-INPUT-FRESH, SIG-BODY-FRESH and FRESH-CLAIM for the DOES> body rather than a second path). Ownership: checker.f CHECK-DOES-RUN. Claim: unassigned.

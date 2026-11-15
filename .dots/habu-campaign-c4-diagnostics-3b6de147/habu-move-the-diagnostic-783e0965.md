---
title: Move the diagnostic tools onto the one form
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.376241+01:00"
---

Problem: tools/check-core.f CHK-PREVERIFY-FAIL prints three check.f: lines; src/lib/verify-diagnostics.f and tools/check-all-errors-core.f hand-build JSON; lsp-diag and gate-json-assert read schema_version, verdict and repair_class. Acceptance: the tools print and read only the campaign's form and keys; the contract text sits in the package header; docs/debugging.md and docs/forth.md describe the form. Depends: habu-report-a-def-2b4ff9c9 habu-report-checker-record-15362e5c habu-give-declaration-refusals-b0c396eb habu-report-the-interpreter-10ba8b10. Claim: unassigned.

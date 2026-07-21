---
title: Document errors.f as global-vocabulary exception
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T06:57:16.425810+02:00"
---

Follow-up to the errors-stay-global decision recorded in habu-pkg-remaining-30-99dbf693 (2026-07-21): lib/errors.f needs the same in-file documentation block that lib/prelude.f gained in 9a8137c8 - a header comment stating it is the second sanctioned core-surface exception to the package-first rule (flat globally-unique throw-code vocabulary like src/config.fs constants; first on the seed prelude so packaging mistakes brick recovery; do not wrap in a package, do not qualify consumers). Also add one line to docs/forth.md § Packages naming both sanctioned exceptions (prelude.f, errors.f) so reviewers do not re-litigate them. Comment/doc-only change; verify host-lint, filemap-lint, and the errors.f load path.

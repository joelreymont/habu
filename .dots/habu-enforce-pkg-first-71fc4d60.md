---
title: Enforce package-first Forth authoring
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T05:00:44.313685+02:00\""
---

Full context: docs/forth.md already requires packages, but the top-level AGENTS.md session contract only points at the document and repeated generated code has still introduced global stem prefixes instead of package-local short names. Cause: package ownership is not an explicit authoring preflight or pre-completion check in the blocking agent instructions. Fix: add a blocking package-first rule to AGENTS.md: choose/open the owner package before defining any non-core project word; use short unprefixed public and private tails inside it; qualify only across packages; permit global definitions only in documented core/prelude boundaries; inspect every changed Forth definition for redundant package-name/file-stem prefixes before commit. Record the concise lesson without duplicating API reference. Acceptance: AGENTS.md contains an operational pre-authoring and pre-commit rule, docs/forth.md remains canonical, LESSONS.md records why instructions alone were missed, and no code/API syntax is duplicated. Files: AGENTS.md, LESSONS.md. Verify: dot-dep-lint, host-lint, filemap-lint.

Claim: agent=root workspace=.jj-ws/habu-package-rule.

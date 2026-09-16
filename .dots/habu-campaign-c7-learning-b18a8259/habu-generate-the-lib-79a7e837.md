---
title: Generate the library reference from signatures
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:54:54.731701+03:00"
---

Problem: docs/stdlib.md is hand-written and drifts from lib/, while tools/public-signatures.f already extracts every public signature and packages carry doc comments; VFX Forth's DocGen shows a reference generated from source stays true. Acceptance: a checked tool that emits one Markdown page per package from public signatures and the package's leading comment block, deterministic across two builds of one tree, written under docs/reference/; a gate that fails when a public signature or doc comment changes without the generated page being regenerated; docs/stdlib.md keeps the prose guide and links to the generated pages. Files: tools/public-signatures.f, tools/docgen.f (new), docs/reference/ (generated), docs/stdlib.md, test/ gate registration. Verify: run twice and diff; the gate red on a deliberate signature edit; test/run.f green. Depends: none. Ownership: docs lane. Claim: unassigned.

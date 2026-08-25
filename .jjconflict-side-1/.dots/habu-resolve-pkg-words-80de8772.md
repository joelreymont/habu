---
title: Resolve package words with trailing colon via qualifier
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T00:44:58.294444+02:00"
---

Name-resolution gap found during codegen-buffer consolidation: a package-public word whose name ends in ':' (e.g. a BUFFER: definer in package CODEGEN) cannot be called across package boundaries as CODEGEN:BUFFER: - the trailing colon reads as a name edge, so resolution fails. lib/codegen.f worked around it by dropping the trailing colon from its definer names (BUFFER / BUFFER-E), diverging from the EXTENT:/NOMINAL:/SPEC: definer naming convention. Decide the long-term rule: either make the qualifier parser resolve a final ':' as part of the word name (fix in the package name-resolution path, with a regression test for PKG:WORD: forms), or codify in docs/forth.md section Packages that definer-style names ending in ':' must be package-local or renamed, and align the convention. Files: package resolution in src/core (locate the qualifier parser), docs/forth.md, regression test.

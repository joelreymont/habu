---
title: Add maki one-way dependency lint
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:56:37.211029+02:00"
---

Build a checked Habu lint that fails if any src/, lib/, or gate file references a maki/ path (enforces PLAN.md Guardrails one-way import: maki->habu, never reverse). Today NO such lint exists (grep of tools/+test/ for one-way/forbidden-import returns nothing), so "enforced" is currently only convention + the gate explicit file allow-lists.
- Files: new tools/maki-dep-lint.f (pattern after tools/trust-lint-core.f scan structure); register in test/gate-stdlib.f TEST-SUITE and FILEMAP.md; add TRUSTED.md row only if a trusted boundary is needed.
- What: lex whole tokens (not substrings - see docs/forth.md "Source-use guards match tokens"), scan src/+lib/+gate source for a literal maki/ path reference, throw a named code on any hit.
- Verify: negative fixture - a src/ file with a maki/ load is rejected; positive - clean tree passes. Strictly typed Habu (CHECKED:), T{ }T tests.
- Dep: none (pure tooling); needed before maki/ scaffold merges gate-clean.

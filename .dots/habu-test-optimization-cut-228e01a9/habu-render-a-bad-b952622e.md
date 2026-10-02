---
title: Render a bad stored signature under --json-errors
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T13:23:51.461268+02:00"
---

src/core/checker.f USIG-ADD-BAD (~8909-8916) dies 76 with a prose line when a stored signature is bad and multi-error mode is off, so tools/check.f --json-errors without --all-errors writes no JSON record for it (review 436; documented at docs/repair-diagnostics.md ~97-100). The trust-row sibling renders its record and throws. Acceptance: in every check.f mode a bad stored signature yields its E-BAD-STORED-SIGNATURE record and a refusal status, by rendering through BADSIG-XT and throwing as the trust row does, seen failing first.

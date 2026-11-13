---
title: One refusal code for package in a public section
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:25:46.239252+02:00"
---

Problem: 'package' written inside a package's public section is refused with rc 75 under 'hb --load' but as E-STATEMENT-THROW 7136 under tools/check.f ('package' runs in src/habu/packages.f on every path). The same program has two refusal codes by load path. The form stays refused: both paths answer one code. Acceptance: the reproducer answers the same code and text under --load and tools/check.f. Files: src/habu/packages.f, tools/check.f's statement path. Verify: the reproducer on both paths. Depends: none. Ownership: TBD by the lane. Claim: unassigned.

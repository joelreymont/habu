---
title: Unjam the tail-pure fork group
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T22:22:04.342182+02:00"
---

The tail-pure fork group of the stdlib gate is red with 19 phases failing on habu: bad enum declaration 'nzcv': name is reserved or already taken — including tools/codegen-workload-test.f, which passes standalone. Verified pre-existing by the scan-fixtures worker on 2026-08-04: pristine sandbox at the parent commit shows byte-identical RED member lists (17 named + 2 unnamed), reproduces with old and freshly rebuilt bin/hb. It is a load-order collision in the group's shared process — two members both declare an 'nzcv' enum (ARM64 flags; look in src/arch/arm64/ and src/compiler/a64-*), or one member leaks it into a scope a later member re-declares in. Work: find the two declaration sites, decide the owner (one canonical enum both import, per the package rules — no rename-to-dodge), fix the loser's reference, and add whatever fork-group isolation rule makes 'passes standalone, red in the group' impossible for enum declarations, or at least a lint that flags duplicate enum names across gate-group members. Acceptance: the tail-pure group green, the member standalone green, and the collision class either structurally impossible or linted.

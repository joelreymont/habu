---
title: Delete duplicate IR manifests and IR census pins
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:45.767176+02:00"
---

Problem: ir-intern, ir-structure and ir-storage manifests restate ir-symbol/type/attr/op/fun/arena/context cases vector for vector; compiler-ir-id's PUBLIC# census (test/compiler/ir-id.f:483-492) is a change detector and 'KIND# 2 * 8 + RAW# T=' (514-517) a tautology. Evidence with cited duplicate lines: ~/.cache/tmp/kestrel-gate/test-review/L2-compiler-other.md. Acceptance: the three manifest rows and their files are deleted after confirming each vector's twin; the census and tautology are removed; registry and references updated. Files/Ownership: test/compiler/ir-intern*, ir-structure*, ir-storage*, ir-id.f, registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: remaining ir-* rows pass; rg shows no references to deleted files. Depends: none. Claim: unassigned.

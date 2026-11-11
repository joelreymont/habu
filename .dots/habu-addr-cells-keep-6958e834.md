---
title: "ADDRESS-CELLS:KEEP-BELOW and REMOVE-SPAN take offsets where an address was chosen"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:35:39.686528+02:00"
---

Problem: Joel chose 'Address, keep package' (2026-11-05): the two words take an address and cut the list of the region holding it, as EMIT-MARK picks it, because an offset from hb's DATA makes a failed line inside a build cut hb's own rows. The code passes offsets: src/habu/interpreter.f:2839, src/habu/address-cells.f:229, :251-253. Acceptance: both words take an address and cut only the region that holds it; a failed line inside a build leaves hb's own rows. Files: src/habu/address-cells.f, src/habu/interpreter.f. Verify: a build whose source has a failed line, then a build by the same hb equal to a cold one. Depends: none. Ownership: unassigned. Claim: unassigned.

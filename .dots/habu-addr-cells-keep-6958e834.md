---
title: "ADDRESS-CELLS:KEEP-BELOW and REMOVE-SPAN take offsets where an address was chosen"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:35:39.686528+02:00"
---

Problem: Joel chose 'Address, keep package' (2026-11-05): the two words take an address and cut the list of the region holding it, as EMIT-MARK picks it (src/habu/habu2.f:2258), because an offset from hb's DATA makes a failed line inside a build cut hb's own rows. The code passes offsets: src/habu/interpreter.f:2483 ('cut base - ADDRESS-CELLS:KEEP-BELOW'), src/habu/address-cells.f:228 KEEP-BELOW, :250 REMOVE-SPAN. Acceptance: both words take an address and cut only the region that holds it; a failed line inside a build leaves hb's own rows. Depends: none. Claim: unassigned.

---
title: Let DERIVE eq and hash cover a structure with a pointer field
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:34:29.143952+02:00"
---

Problem (review of carl-self, probe ~/.cache/tmp/rev-selfptr/p2.f cases D2c, D2o): a STRUCTURE with a ptr field (ptr n, ptr other) that DERIVEs eq or hash fails its generated :eq/:hash check: rc 70, 'expected: pd2c n n actual: pd2c n ptr n … declaration failed'. A linked record (now admitted, dot 7013f491) that derives eq or hash hits it. Acceptance: the cause found in the derive generator (which field kinds it compares and how), and a structure with a pointer field derives eq (address equality) and hash or is refused with a message naming the pointer field; decided by the cause. Files: the DERIVE generator (src/core/structure-decl.f / generated-declaration*.f). Verify: p2 D2c and D2o; structure-decl-suite rc 0. Depends: none. Ownership: derive generator. Claim: unassigned.

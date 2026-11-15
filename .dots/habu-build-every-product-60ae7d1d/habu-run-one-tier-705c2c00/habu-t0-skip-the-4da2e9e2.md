---
title: "T0: skip the store guard on constant addresses outside every band"
status: open
priority: 3
issue-type: task
created-at: "2026-11-06T14:51:54.224269+02:00"
---

Problem: carl measured (2026-11-06, base hb 590a52d7, dot habu-find-why-checker-867bc4bb on master 82990fc6) 9457 guarded stores per checked definition, 7824 to DATA at or above DATA-START and 1632 to map-anon arenas below DATA, none in a protected band: about 129k of 1.26e6 instructions per definition (about 10% of checking). Tier 1 compiles a non-tail `!` to a variable's constant address as mov x10,<addr>; mov x11,#8; bl (PROT-SPAN); str (emit.f PUT-PSTORE). Acceptance: T0 omits the guard call for a store whose address is a compile-time constant outside every protected band, keeps it otherwise; a store into a protected band still refuses; checker cost per definition measured before and after on spark. Files: T0's store emission (src/habu/t0*.f), named in the brief once designed. Verify: the guard rows and a per-definition instruction count. Depends: the T0 release (speed after release, Joel 2026-10-30). Ownership: jerry. Claim: unassigned.

---
title: Price the pool refusals that zero-arity call sites create
status: open
priority: 2
issue-type: task
created-at: "2026-08-09T23:10:26.150344+02:00"
---

Measured 2026-08-09: admitting ( -- )-with-call routines moves +43 definitions from E-A64SEL-CALL into E-A64RA-POOL (and +15 into E-A64RA-SPILL, +6 E-A64RAV-DKEEP): their live values all cross the data stack, which is exactly the traffic the post-cut register convention (habu-pass-args-in-da01bd62) removes and the caller-save narrowing (habu-close-a-routine-4055b7c7) relieves. DO NOT patch the allocator for these now - the convention deletes that work. After the convention lands, re-derive the count on the tree of the day and decide what remains. Depends: habu-pass-args-in-da01bd62.

PRICED 2026-08-13 by the poolclose lane on master 3cdb1188; the numbers and the
method are written up on habu-close-a-routine-4055b7c7 and this leaf needs no
second measurement. In short: E-A64RA-POOL is 149 rows and E-A64RA-SPILL 116;
narrowing the caller-save against the callee's clobber record would close 149
and 115 of them, but it closed none of the rows measured, because the call those
bodies cross is a reference to a named `constant` - which elaborate.f resolves
as a callable and which will therefore never have a record. On a text scan 131
of the 149 are that shape. Neither the narrowing nor the register convention
owns it; folding a named constant to its value does.

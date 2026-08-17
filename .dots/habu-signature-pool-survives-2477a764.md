---
title: Signature pool survives rollback pops
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-17T15:30:42.484364+02:00\""
---

Audit major (2026-08-17): RBF-POP rewinds SYM-N and retires the sym-keyed hash index (checker.f:12631 HIDX-SYMS-RETIRE - the tree KNOWS sym rewind invalidates sym-keyed caches) but no ASIG mark is in the frame: ASIG-ROW-U/ASIG-STR-U/ASIG-LAST (sym-keyed, :4984) keep rows written inside a popped scope. Reached today: every SUMTYPE/STRUCTURE/ENUM in an armed window runs TDPLAN-PREFLIGHT-CHECKER inside a candidate frame, captures rows, pops; the re-interned real definitions reuse rewound sym ids. Deterministic realignment + the byte-identical dedup skip makes it CORRECT BY LUCK; a misalignment makes ASIG-LAST@ answer a DIFFERENT word's row, silently defeating CHECKER-ASIG-MISSING? - the audit built to fail loudly. Fix shape (mirrors existing code): retire ASIG-LAST entries at/above the restored SYM-N in RBF-POP, or carry ASIG marks in the frame while armed. Regression: a preflight-then-misalign forge proving the stale row is unreachable; mutation = drop the retire, red by name.

Claim: agent=audit-close workspace=.jj-ws/habu-audit-exec

FIXED 2026-08-17. RBF-POP-WITH calls ASIG-SYMS-RETIRE beside the
HIDX-SYMS-RETIRE it already ran, on the same restored SYM-N: the index
is retired, the ROWS are not (a row records the name, package and
visibility it was written under, so it stays true about the word it
names; what cannot survive a rewind is the claim that it is symbol N's
newest). Regression test/checker-rollback-sig-pool.f drives
CHECKER-CANDIDATE-SCOPE-START / CHECK! / -DONE - the preflight's own
entry points - and BREAKS THE REALIGNMENT with one extra symbol between
the pop and the real definitions, so the ghosts' ids go to different
words. Measured on the unfixed engine: RBSIG-SHIM answered a row named
rbsig-ghost-a, RBSIG-REAL-A answered rbsig-ghost-b, and the pool held
two rows for four words; disarmed, CHECKER-ASIG-MISSING? answered false
for two words the pool has no row for. Mutations: dropping the retire
reds 7 assertions across two cases by name; retiring from zero reds
KEEP-CASE and also stops the BUILD, with the production capture audit
naming it ("aot-capture: the checker knows an effect for window word
MAKE and the captured signature pool carries none").

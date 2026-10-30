---
title: Refuse a malformed quotation at tier 1 with a caught code
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T21:09:40.575258+02:00"
---

Finding (ctrl lane, workspace .jj-ws.noindex/jerry-ctrl): unchecked ': GEKT ( n -- n ) [: 0= if 7 ;] then ;] execute ;' at tier 1 throws E-NELAB-QUOT (-8651), which src/compiler/native/definition.f MALFORMED? does not list, so the load dies uncaught (rc 67) where T0 refuses it rc 70 'ncomp: cannot compile GEKT at then'. E-NELAB-QUOT covers both malformed [: ;] and genuine elaboration gaps, so adding it to MALFORMED? as is would also turn real gaps into refusals: split the malformed case into its own code first. Probe: /tmp/jerry-ctrl-tmp/pr/q2.f. Not a T0 release prerequisite (tier 1 stays for experiments).

Parity run /tmp/rev-feat/par/c.out (2026-10-31): tier 1 also ends rc 67 on a well-formed quotation, `[: 1 ;] drop 1`, in :, TRUSTED: and 0 set-check bodies, where the JIT and T0 compile it: the genuine elaboration gap the split must keep apart.

Censuses 2026-10-31 (/tmp/jerry-big/t0cmp-A.md, t0cmp-B.md): tier 1 also ends rc 67 with no token on unchecked depth faults at joins (-8503: if/until/loop/leave/case depth), underflow (-8304), exit arity (-8303), of-orphan (-8650), an out-of-range literal in every definer (-8405), is PLAIN/NOSUCH-D/IMM1 (-8653), MATCH without OF, unknown construct (-8650): each where T0 (after the census lanes) or the JIT prints a diagnostic.

Lane t0fe (2026-10-31): tier 1 also ends rc 67 (uncaught -8651 E-NELAB-QUOT) on a quotation with dead code, `[: 1 exit 2 ;] drop 3`, where T0 refuses it rc 70.

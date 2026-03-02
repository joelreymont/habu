---
title: Audit and harden VM auxiliary control-stack capacity
status: active
priority: 1
issue-type: task
created-at: "\"2026-03-07T19:32:55.766619+01:00\""
---

src/interp/vm.zig:842-847, 2687-2688, 5861-6006, 11524-11525; ../maxima/src/limit.lisp:119-260; ../maxima/src/defint.lisp. Root cause: MAX_CATCHES/MAX_UNWINDS may block limit/defint independently of MAX_FRAMES. Fix: identify the first overflowing control stack on current repros and either raise limits safely or redesign growth with correct GC root scanning. Why: this is the load-bearing Stage-3/4 VM prerequisite in the reviewed plan.

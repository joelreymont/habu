---
title: "Make a word defined by evaluate text visible to the verifier's binding check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:58:14.261698+02:00"
---

Problem: the source verifier asks the checker what a top-level token binds (src/habu/verify-source.f:2835 TOP-BINDING), and a word defined by `evaluate` text is invisible to that answer. `package P s" : dup ( n -- ) drop ;" evaluate : X ( n -- ) dup ; ;package` gives --verify-only rc 70 and --load rc 0; and `s" : 0= ( bool -- bool ) drop false ;" evaluate` before `HB-TARGET-LINUX? 0= [if] ... [then]` makes CORE-OP? (verify-source.f:3962) take the program's 0= for the engine's. Acceptance: for both programs --verify-only and --load give the same answer; a definition made by evaluate text binds for the verifier as it does for the load, or the verifier defers from the evaluate on. Depends: the design of how the verifier knows an engine word may absorb this. Claim: unassigned, owner carl.

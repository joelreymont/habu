---
title: "Make a word defined by evaluate text visible to the verifier's binding check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:58:14.261698+02:00"
---

Problem: the source verifier asks the checker what a top-level token binds (src/habu/verify-source.f TOP-BINDING, about :2993), and a word defined by `evaluate` text is invisible to that answer. Shown by the host-loads re-review (2026-11-10, lane carl-aio 61534ce8; older than the lane): `package P s" : dup ( n -- ) drop ;" evaluate : X ( n -- ) dup ; ;package` gives --verify-only rc 70 and --load rc 0; and `s" : 0= ( bool -- bool ) drop false ;" evaluate` before `HB-TARGET-LINUX? 0= [if] ... [then]` makes CORE-OP? take the program's 0= for the engine's. Acceptance: for both programs --verify-only and --load give the same answer; a definition made by evaluate text binds for the verifier as it does for the load, or the verifier defers from the evaluate on. Files: src/habu/verify-source.f, src/core/checker.f (expected). Verify: the two programs through `--load tools/check.f -- --verify-only` and `--load`. Depends: the design of how the verifier knows an engine word (brief .jj-ws.noindex/carl-layout/.out/briefs/design-engine-word-identity.md) may absorb this. Ownership: carl. Claim: unassigned.

---
title: State the real exit of an uncaught throw in the docs
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T16:51:16.740146+02:00\""
---

Problem: docs/gate.md:169-172 and docs/debugging.md:138-142 say an uncaught throw exits with the code's low eight bits and prints nothing (56 for -2504). Measured 2026-09-30: a code in 1..255 exits with that code silently; every other code prints 'hb: uncaught throw code N' on stderr and exits 67 (src/habu/habu2.f LUNCAUGHT). Acceptance: one correct statement in docs/debugging.md, docs/gate.md points to it, no second copy. Files: docs/gate.md, docs/debugging.md. Verify: `: M ( -- ) -2504 throw ; M` under --load exits 67 with the message; 42 exits 42 silent. Depends: none. Ownership: those two paragraphs. Claim: agent=kestrel workspace=.jj-ws/habu-r4.

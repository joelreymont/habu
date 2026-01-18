---
title: Fix psetf lambda-in-mapcar issue
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-18T06:24:12.819601+02:00\""
---

Files: stdlib.habu:1847
The mapcar lambda in psetf macro is causing compilation failure.
Either rewrite psetf to not use lambda, or fix lambda compilation in macro expansion.
Depends: habu-debug-repl-vs-7a89f91c (to confirm this is the issue)
Verify: ./zig-out/bin/habu loads without error.
Est: 25min

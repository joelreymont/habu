---
title: "Refuse a NUL inside a raw spawn's strings"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T15:53:44.091830+02:00"
---

Problem: the PROC-RAW family copies argv, env and cwd strings unchecked (PROCESS:ZCOPY, src/lib/process.f:203-211) and measures them with ZLEN, so an embedded NUL truncates silently: PROC-ARGV+ of 'a<NUL>b' makes /bin/echo print 'a', and a cwd of '/tmp<NUL>x' runs in /tmp. The request layer refuses a 0 byte (src/lib/spawn.f:97-99, E-FFI-NUL). TEST-CAPTURE-SPAWN-THROWS (process-test.f) uses that truncation as its in-spawn refusal and must keep a trigger inside the spawn's catch. Acceptance: the RAW family refuses an embedded NUL with E-FFI-NUL; the capture-throw test still throws from inside the spawn. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-nul.

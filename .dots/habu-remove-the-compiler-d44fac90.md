---
title: Remove the compiler lease
status: active
priority: 1
issue-type: task
created-at: "2026-11-03T09:30:01.242098+02:00"
---

Joel 2026-11-03: "if you are taking no lease then why are you modifying calls to NLEASE? why have NLEASE at all?" Compiles are single-threaded; NLEASE's checks repeat exit 79 (task thread), NDEFINITION's M-OPEN (compile inside a compile) and NSESSION's WORK-OWNER (backend work). Design: /tmp/jerry-big/nolease-design.md (rev 1, at critic): delete src/compiler/session/lease.f and every use (compiler.f, backend.f, shadow.f, kernel-hir-x64.f, IR-CTX's admission hook, six test files, card line, src/compiler/CLAUDE.md, LESSONS entry). Public items removed or retyped: NLEASE package, NSESSION:session's lease field, NSESSION:NEW, NBACK:PROTOTYPE, NBACK:FORGET, NCOMP:SESSION-LEASE, IR-CTX:SESSION-STAND-DOWN!. Next: critic, Joel's approval of the public items, then one worker lane in .jj-ws.noindex/jerry-nolease. Supersedes the open finding in habu-run-t0-compiles-dd3d9b8a (NSHADOW:OPEN/CLOSE drop their lease check). Claim: jerry.

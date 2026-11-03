---
title: Remove the compiler lease
status: active
priority: 1
issue-type: task
created-at: "2026-11-03T09:30:01.242098+02:00"
---

Joel 2026-11-03: "if you are taking no lease then why are you modifying calls to NLEASE? why have NLEASE at all?" Compiles are single-threaded; NLEASE's checks repeat exit 79 (task thread), NDEFINITION's M-OPEN (compile inside a compile) and NSESSION's WORK-OWNER (backend work). Design: /tmp/jerry-big/nolease-design.md, revision 4 after three critic REJECTs (rev 3 asked for FLOAT-REFUSAL's comment reason, the table's callers and errors, and what a compile started inside NSESSION work does); all three accepted the core argument that every lease refusal is made by NDEFINITION:IDLE-CK, IR-CTX's SESSION-DEEPEST-CK or REGISTRY-EMPTY-CK, or NSESSION's WORK-OWNER, or is reached only by the lease's own tests. Rev 4 is at critic. Also deletes FIXED-LEASED, which the Intel commit cf52b51d (dot habu-scope-intel-fixed-f616a52d) edits; that change moves to COMPILE-FIXED (master wins). delete src/compiler/session/lease.f and every use (compiler.f, backend.f, shadow.f, kernel-hir-x64.f, IR-CTX's admission hook, six test files, card line, src/compiler/CLAUDE.md, LESSONS entry). Public items removed or retyped: NLEASE package, NSESSION:session's lease field, NSESSION:NEW, NBACK:PROTOTYPE, NBACK:FORGET, NCOMP:SESSION-LEASE, IR-CTX:SESSION-STAND-DOWN!. Next: critic, Joel's approval of the public items, then one worker lane in .jj-ws.noindex/jerry-nolease. Supersedes the open finding in habu-run-t0-compiles-dd3d9b8a (NSHADOW:OPEN/CLOSE drop their lease check). Claim: jerry.

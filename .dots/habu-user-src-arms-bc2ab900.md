---
title: "User source rebinds the engine's defers"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:25:35.107622+02:00"
---

Problem: a program names and rebinds 56 engine defers with `is`: 43 global in src/core/checker.f, eight public in VERIFY (src/habu/verify-source.f), TYPE-DECL's TDECL-TXN-XT, TDECL-NAME-PREFLIGHT-XT and TDECL-CAPACITY-PREFLIGHT-XT (src/core/sumtype.f), TFAM-ADDR-WORD-XT (src/core/type-family.f) and REPL-READ (src/habu/repl.f:32). It also installs its own xt through USES-INSTALL (checker.f:10423), CHECKER-CURSOR! (:10533), CHECKER-RESOLVE:EACH-VISIBLE (:20266) and TYPE-DECL:TXN-INSTALL (sumtype.f:983). Reproducers, each rc 0 under --load and under tools/check.f: `SUMTYPE probecolor VARIANT red ;VARIANT ;SUMTYPE` / `: MYAR ( n -- n ) ." MYAR ran" cr drop 0 ;` / `: SETIT ( -- ) ['] MYAR is TFAM-ARITY-XT ;` / `SETIT` / `: G ( probecolor -- ) drop ;` prints "MYAR ran" four times; `: MYPRE ( ^u8 n -- ) ." MYPRE ran on " type cr ;` / `: SETIT ( -- ) ['] MYPRE is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;` / `SETIT` and a SUMTYPE declaration runs MYPRE. Setters outside the engine: src/lib/genio.f:162 (REPL-READ), tools/check-verify-child-lib.f (VERIFY's ON-* and SUPPLIED, ten sites) and tools/check-core.f:1366 (PROCESS:STOP). A write through a private `CAST:` pointer is the writer's claim (docs/forth-card.md, CAST:) and outside this dot.
Acceptance: both reproducers are rejected as undefined under --load and under tools/check.f, as a test; user source can name no defer whose binding decides what the checker, loader or compiler accepts or records, and no word that installs an xt into one. REPL-READ (genio's line source), VERIFY's ON-* observers and SUPPLIED (tools/check-verify-child-lib.f) and PROCESS:STOP (tools/check-core.f) stay public: they supply text or observe, and decide no verdict.
Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub.

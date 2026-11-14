---
title: "User source arms the storage definers' window and mints a typed pointer"
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T19:25:35.107622+02:00"
---

Problem: user source reaches the storage definers' window and the checker's trust, each rc 0 on the typed-children engine:
A. LBUF-EVAL and LBUF-EVAL! (src/core/layout-buffer.f:135, :134) stay callable after src/core/layout-buffer-seal.f erases LBUF-PEND! and its siblings: 'PRODUCT pr2 FIELD x n FIELD y n ;PRODUCT' / 's" create FOOB 1 allot : FOO ( n -- ptr pr2 ) FOOB swap 16 * + ;" s" FOO" LBUF-EVAL! drop' / ': USE ( -- ptr pr2 ) 1000 FOO ;' loads (tools/check.f refuses it), and USE yields a ptr pr2 16000 bytes past a 1-byte buffer. src/core/arena.f EVAL :48, src/core/heap.f :56-57 and src/compiler/workspace.f DECLARE :58 call LBUF-EVAL! and load after the seal. The comments at src/core/checker.f:4214 and :22719 ('user source cannot arm it') are false.
B. `is` on an engine defer: ': SETIT ( -- ) ['] MYEVAL is TYPE-DECL:TDECL-EVAL-XT ;' loads and checks, since IS-TOK (src/core/checker.f) checks only the effect; with MYEVAL evaluating a forged FOO, a later '4 TYPED-BUFFER FOO n' installs it inside the window. TDECL-EVAL-XT (src/core/sumtype.f:1053) is also the evaluator sumtype's own generators call; TDECL-NAME-PREFLIGHT-XT, TDECL-CAPACITY-PREFLIGHT-XT and STORAGE-CLEAR-XT are untried.
C. ARENA splices its package name (CHECKER-AUTH-PACKAGE$) unescaped into the generated NEW word (src/core/arena.f SOURCE): a package named P"Z, whose Z defines QZ", makes QZ-NEW hand out ptr pr2 records 1 byte apart (not rerun on this engine).
D. 'PRIM: FOOB PE-PTR-N PE-OUT PRIM;' in user source loads and checks: it gives an engine word's row to a name of its choice. SOURCE-UNIT:USE is likewise reachable (not carried further).
With no generated text (habu-bind-generated-structure-0124b1b5) A's and C's text and TDECL-EVAL-XT go; `is` on the other engine defers and PRIM: stay.
Acceptance: each program above is refused under --load and under tools/check.f, as a test; no word user source can call opens the window or rebinds an engine defer; the engine's own PRIM: rows still load; the storage definers still build; the two comments are true.
Depends: none. Claim: unassigned.

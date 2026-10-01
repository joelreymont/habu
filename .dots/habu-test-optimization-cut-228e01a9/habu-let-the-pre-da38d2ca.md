---
title: "Let the pre-verifier admit ['] and keyword locals"
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:24:37.410400+02:00"
---

Problem: the baked pre-verifier (src/habu/verify-source.f:287-293, BODY-PARSER? and TOP-PARSER?) refuses sources the engine loads. (1) `: \ ( -- ) ;` then `: F ( -- [ -- ] ) ['] \ ;` loads rc 0 and prints 7, but check.f refuses it rc 74 "unterminated definition" ($HOME/.cache/tmp/kestrel-r4-lexrec/c3d/t1.f): ['] is not a body parser there, so its operand is read as a word. (2) Locals named after a parsing keyword (c3b/l1.f, l2.f) load rc 0 and check rc 74. Adding ['] to BODY-PARSER? alone breaks c3b/l3.f (a local named ['], which checks today): the engine and the checker's body reader look a local up before the keyword (LOC-REF? before BTICK-CAND? and PARSE-LIT?), so the pre-verifier needs the same locals scope (as discovery's SD-LOCAL*/SD-SCOPE* model it). Found by the r4-lexrec c3 worker (brief 129). Acceptance: t1, l1, l2, l3 and l4 each give the same verdict under check.f as under the loader, failing cases written first through the real check.f path; a parsing keyword outside its own state still refuses as it does now; rebuild, g1 = g2 with .names, two-generation build. Base: after lexrec c3 (dot 1bed26df), which adds the shared parsing-keyword rule in lib/source.f. Files: src/habu/verify-source.f and the suite that owns check.f (tools/check-test-lib.f).

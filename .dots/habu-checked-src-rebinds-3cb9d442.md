---
title: "Checked source rebinds the storage definers' evaluator with is"
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T19:45:24.890759+02:00"
---

Problem: ': SETIT ( -- ) [''] MYEVAL is TYPE-DECL:TDECL-EVAL-XT ;' is accepted: IS-TOK (src/core/checker.f) checks only the stack effect. With MYEVAL = '2drop s" : FOO ( n -- ptr pr2 ) FOOB swap 16 * + ;" INCLUDE-EVALUATE', running SETIT and then '4 TYPED-BUFFER FOO n' installs the forged FOO inside the storage window: rc 0 under --load and under tools/check.f. The same 'is' is admitted on any engine defer (TDECL-NAME-PREFLIGHT-XT, TDECL-CAPACITY-PREFLIGHT-XT, STORAGE-CLEAR-XT untried). Run by the hunt of habu-user-src-arms-bc2ab900 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/storage-window-hunt.md, door B); not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: user source cannot rebind a deferred word the engine owns; the program is refused at 'is' with a reason. Design in work with habu-user-src-arms-bc2ab900 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Design-storage-window.md). Files: named by the design. Verify: the program, refused under --load and tools/check.f. Depends: the design. Ownership: who may rebind an engine defer. Claim: unassigned.

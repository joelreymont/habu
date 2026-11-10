---
title: Locate the engine's interpret-level refusals
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T14:31:40.370636+01:00"
---

Problem: ten interpret-level refusals in src/habu/habu2.f EM-COMPILE-UNDEF (:12703-12781: LWIDE, LINTERNAL, LTRUSTTICK, LMININ, LPREFMISS, LORPHAN, LCFKIND, LCFCAP, LLOCWIDE, LLOCMANY) print their message, the token and a newline (LPREFMISS a message with a baked newline and no token), then branch to the shared LDIAGRET tail (:12708), which prints no place; LFLOORREC (:12748-12753, a data-stack underflow caught by the guard page) prints the same way and throws through LEVALREC, also with no place. An undefined word instead branches to LCOMPILEDIE (:12807), whose LSRCAT (:12818) prints ` at <path>:<line>`. Their Habu twin, src/habu/outer.f REFUSE (:349-351), likewise prints message and token only, though outer.f AT-SOURCE (:359) prints the place as LSRCAT does. It is an acceptance case of its parent, habu-move-the-interpreter-d35780fa: fix in the code that stays, never in the machinery the parent replaces. Split from this dot: habu-a-refused-declaration-6fe9ce8d, habu-hb-and-check-c9dd044e, habu-an-uncaught-0-1798b11a, habu-a-plain-program-491d1dd9, habu-a-checked-body-e57cadb7; bare library throws and the Maki case are habu-uncaught-throws-name-1a43b055's.
Acceptance: each of these eleven refusals, in the engine and in outer.f, prints its file and line through the one located tail (LCOMPILEDIE and LSRCAT, AT-SOURCE in outer.f), with its exit status and throw code unchanged.
Files: src/habu/habu2.f, src/habu/outer.f, the tests that pin these messages.
Verify: a file holding the one line `then` (an orphan closer, LORPHAN) under bin/hb --load on a rebuilt hb prints its path and line 1 and exits 70.
Depends: none.
Ownership: unassigned. Claim: unassigned.

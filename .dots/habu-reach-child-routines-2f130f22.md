---
title: Reach child routines without a private name
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T19:47:48.202487+01:00"
---

Problem: a child word's body calls a routine its declaration's package keeps private, and the engine finds that routine by name at run time: src/habu/interpreter.f CHILD-BODY (:2244-2266) passes CHILD-ROUTINE (:2240) the handles 'private LBUF ELEMENT', 'BIND', 'GROW', 'private DYNAMIC-STORAGE ELEMENT', 'ENSURE', 'EMPTY', 'private TASK SLOT', 'SLOT-AT', and HANDLE-REC (:1996-2003) probes the package's private wordlist for each. The same by-name probe of an engine private serves postpone: src/core/checker.f POSTPONE-RUN-REC (:21612-21621) finds INTERPRETER's private postponed, and interpreter.f POSTPONE-RUN (:2018) resolves a 'private PKG NAME' handle through HANDLE-REC. An hb whose private names are stripped (habu-re-enable-hb-a2404c26) therefore cannot load a declaration of these shapes: the stripped hb2 the strip lane builds (.jj-ws.noindex/jerry-strip, commit 419156e3) fails loading Tender's tender.f with rc 70 'E-UNDEFINED ... src/lib/fork-child.f:16: undefined word ELEMENT'. Acceptance: no path reached from loading checked source looks up an engine package's private record by name; a stripped hb loads tender.f, every TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER, +USER and +USERS declaration and every postpone of a private word, with the child bodies and postponed calls the unstripped hb makes. Depends: none. Claim: unassigned.

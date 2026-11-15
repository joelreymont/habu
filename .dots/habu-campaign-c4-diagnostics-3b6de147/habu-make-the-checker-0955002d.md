---
title: "Make the checker's catchers keep or stop"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.473265+01:00"
closed-at: "2026-11-16T04:07:35.534605+01:00"
---

Problem: the catchers CHECK-DEF-THREW (check-walk.f:1881), SRC-DOES-THREW (:2230), CHECKER-VERIFY-TOP (:2313) and SKIP add to TALLY after catching; DEF-REFUSAL, REFUSE-DEF, DEF-STOPPED and CHECKER-DEF-STOPPED? form a flag machine; REPORTED-ERROR? (:945-947) and CAST-ERROR:SKIP (:979-981, called at verify-source.f:2449) ask whether a code was reported; REPORT-DEFERRED (check-hook.f:564-568, 586) drops a rejection under MULTI-ERR?. The Design says a catcher never prints and never asks whether a code was reported. Acceptance: a rejection KEEPs under MULTI-ERR? and STOPs otherwise at its writer; the flags, REPORTED-ERROR? and the after-catch counts are gone; under --all-errors a deferred rejection prints. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

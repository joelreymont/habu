---
title: Def-compile failure under catch+evaluate crashes
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T15:45:19.657078+02:00"
---

Pre-existing engine defect found in the TFAM 9 slice-2 lane (fable-tfam12). A definition whose ENGINE COMPILE fails inside [: ... INCLUDE-EVALUATE ;] catch crashes with a SIGBUS register dump instead of a catchable throw. Minimal repro (no construct needed): stdin file defining TCE-CATCH (test/type-ctor-suite.f shape: [: INCLUDE-EVALUATE ;] catch) then s" : XG1 ( -- ) qwertyuiop ;" TCE-CATCH -> prints E-UNDEFINED: qwertyuiop then habu-crash regs dump, process rc 134 (SIGBUS). The same bad definition on plain stdin or --load exits ORDERLY rc 70 - only the die/unwind path crossing the nested evaluate input state under an active catch frame crashes. Suspect: E-UNDEFINED die path in the native compiler (habu2.f) aborts across the INCLUDE-EVALUATE input save/restore instead of throwing through the catch frame. Fix: make definition-compile failures inside evaluate throw a catchable code (restoring evaluate input state), add an engine-gate regression (GE-RUN-STDIN case with the TCE-CATCH wrapper expecting a caught code, not rc 134). Until fixed, suites must not TCE-CATCH failing DEFINITIONS (interpret-level failures and declaration throws are fine); TFAM 9 slice 2 routed its engine fail-closed pin through GE-CONSTRUCT-PENDING (plain stdin rc 70) instead.

---
title: Interpret error under catch+evaluate returns 0
status: open
priority: 2
issue-type: task
created-at: "2026-07-09T15:45:22.084289+02:00"
---

Pre-existing engine gap found in the TFAM 9 slice-2 lane (fable-tfam12). An INTERPRET-mode failure inside [: ... INCLUDE-EVALUATE ;] catch prints its diagnostic but the catch returns 0 (success): s" qwertyuiop" TCE-CATCH -> prints E-UNDEFINED: qwertyuiop, catch sees 0. The same input on plain stdin or --load exits rc 70 (fail-closed). The evaluate boundary swallows the interpret error instead of propagating a throw code, so any tool probing evaluate outcomes via catch reads success for a failed interpret - error masking at a TRUSTED boundary (src/core/include.f INCLUDE-EVALUATE). Fix: propagate interpret-level failures inside INCLUDE-EVALUATE as a catchable throw code (matching the rc-70 load-path contract), then pin with a fixture asserting nonzero from TCE-CATCH on an undefined interpret token. Related but distinct from the def-compile SIGBUS crash dot (same boundary, different failure path).

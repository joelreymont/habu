---
title: Let the unify worklist grow past 1024 pairs
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T14:47:30.150201+01:00"
---

Problem: src/core/checker.f:2270 MAXUWL sizes the unify worklist (UWL and seven parallel arrays) at 1024 cells with no grow path, and U-PUSH (:2319) dies 76 when it fills. A correct program fills it: PARAM-PAIR-ARGS (:4428) pushes every argument pair of a parametrised type at once, so a family of 23 parameters nested 24 deep in its last argument (`NEWTYPE wide<a,...,z>`), passed from one word to another, dies "checker: unify worklist full" (reproducer /tmp/jerry-uwl/r5*.f, run.sh, gen.py). FORALL-OPEN-ADD (:4460) has the same fixed cap. Acceptance: both worklists grow with their input, the reproducer checks at any nesting the effect grammar accepts, and a test keeps the 24-deep case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-uwl.

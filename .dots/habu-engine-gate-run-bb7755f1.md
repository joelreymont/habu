---
title: "Engine gate: run runtime slice on candidate"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T05:04:42.065934+02:00\""
---

Full exact gate after checker-preflight rebase exposed test/gate-engine-lib.f runtime helpers hardcoding bin/hb while the resident worker sets HABU_UNDER_TEST/GE-HB$ to the rebuilt candidate. Result: candidate runtime slice exercised stale installed binary and failed residual-compile recovery despite the candidate passing the exact reproducer. Replace subject hardcodes with GE-HB$; retain explicit GE-BIN-HB baseline helpers only where baseline is intended. Add a subject-routing regression, run runtime slice on imported candidate, then exact full gates. Cause evidence: test/gate-engine-lib.f:1222-1698; test/gate-common-lib.f:378-386; failure rc75 at GE-RAWEXIT-RECOVER.

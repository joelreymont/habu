---
title: "Engine gate: run runtime slice on candidate"
status: active
priority: 2
issue-type: task
created-at: 2026-07-17T05:04:42.065934+02:00
---

Full exact gate after checker-preflight rebase exposed test/gate-engine-lib.f runtime helpers hardcoding bin/hb while the resident worker sets HABU_UNDER_TEST/GE-HB$ to the rebuilt candidate. Result: candidate runtime slice exercised stale installed binary and failed residual-compile recovery despite the candidate passing the exact reproducer. Replace subject hardcodes with GE-HB$; retain explicit GE-BIN-HB baseline helpers only where baseline is intended. Add a subject-routing regression, run runtime slice on imported candidate, then exact full gates. Cause evidence: test/gate-engine-lib.f:1222-1698; test/gate-common-lib.f:378-386; failure rc75 at GE-RAWEXIT-RECOVER.

Independent review of pushed revision 78b268cd found the literals replaced but no durable subject-routing regression. Add a source-shape or executable regression that fails if runtime subject helpers revert from GE-HB$ to literal bin/hb, while preserving the explicit baseline helper. Claim: agent=root workspace=.jj-ws/habu-checker-preflight-sol.

Rebase onto master d42b1878 added four package-scope recovery helpers below the runtime-subject marker with literal bin/hb, so the new regression correctly failed before running the candidate. Route all four helpers through GE-HB$. An apparent duplicate GE-PROCESS-PTY call was overlapping inspection output; the source has one invocation. Acceptance: the source-shape assertion and exact runtime slice pass with HABU_UNDER_TEST bound to the freshly rebuilt candidate.

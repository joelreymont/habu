---
title: Embed store in model, delete resident
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:19:34.564791+02:00"
closed-at: "2026-08-02T16:39:49.965561+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
---

COORDINATION PARENT (reshaped 2026-07-26 per the resident-order correction). Why: measured on master - STRUCTURE box 0 FIELD s WSTORE:store ;STRUCTURE declares rc 0, so the linear-payload capability removed the entire premise for WSTORE:resident; the rationale commit 38c9b90f claiming a field cannot carry the three-cell store is factually false. The implementation is two leaves in strict order: habu-cut-gpt2-model-445a19ff (the model field changes to WSTORE:store, construction/disposal/tests in one compiling commit, consuming the EXISTING WSTORE surface) then habu-delete-resident-and-05c594cb (resident, HOLD, park/unpark erasures, pre-reserved cells, RESIDENT-DISPOSE, tests and TRUSTED rows deleted after a zero-reference sweep; the obsolete scoped-resident dot closes; the resident dot becomes landed history). The GPT2TX cleanup-ladder leaf runs only AFTER the cutover so it straightens surviving code. This parent closes when both leaves land.

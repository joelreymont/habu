---
title: PROMOTE passes with constant gates and a self-compare golden
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.975109+02:00"
---

Problem: maki/cad.f:1259-1261 CERTIFY-INTO is a constant PASS with the warn text 'model-level legality only'; :1277-1278 PROFILE-INTO is always NOT-RUN (even on a device) and :1346-1348 declares it non-blocking; maki/golden.f:56-70,97-104 off-device GOLDEN re-executes each node from its own inputs and compares with itself under 0.000001 (GO-SELF?); PROMOTE-OK? (:1349-1353) = constant CERTIFY and self-GOLDEN and GRADCHECK-not-fail (not-run counts); :1401-1410 then writes durable evidence and schedule rows, and TILE-REPLAY (:1239-1242) replays them as truth. docs/proofs.md: a result that restates the model's own definition constrains nothing. Acceptance: CERTIFY-INTO/PROFILE-INTO deleted until they compute something; GOLDEN returns V-NOTRUN when neither an external artifact nor a device leg ran; PROMOTE requires an external or device golden; a test shows a model with no external evidence cannot promote. Files: maki/cad.f, maki/golden.f, maki/gate.f. Verify: maki/test.f. Depends: none. Ownership: maki promotion. Claim: unassigned.

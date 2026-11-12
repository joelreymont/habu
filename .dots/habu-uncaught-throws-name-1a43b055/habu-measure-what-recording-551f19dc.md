---
title: Measure what recording throw places reaches and costs
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T10:09:43.131162+02:00"
---

Problem: the design in the parent leaves these undetermined, and each decides how the build is cut: whether every throw in a linked arm64 image passes src/habu/t0.f DO-CALL today, in particular top-level lines and files loaded before the checker switches on; whether the require registry's stored path is relative to its root on every load route; whether a stripped program keeps the rows ERROR:CODE records when it never calls ERROR:NAME$; how the Wasm kernel throws; what one recorded throw costs in time; and how much an image grows (estimated at about 30 bytes per compiled throw, about 170 KB on a 5.8 MB hb, from a text count of 5589 mentions of throw in src). Acceptance: each answered with its evidence and recorded here; no product code lands. Files: none; probe sources under a temporary directory. Verify: the recorded answers and the probes that produced them. Depends: none. Ownership: measurement only. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-throw.

---
title: Add Maki Orin device gate
status: open
priority: 1
issue-type: task
created-at: "2026-07-01T22:30:47.545579+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
---

File: PLAN.md:657; cause: maki/README.md currently owns the CPU/off-device gate, but the reviewed plan requires a separate canonical Orin Maki-device/capstone gate for maki/gpu, eval-device, device lowering, and maki/gpt tests; fix: document and wire the Orin-only gate with named off-device SKIP behavior and all new Maki device/capstone entries; deps: fail-closed PTX device runtime; verification: the README command runs on Orin, CPU-only gate still passes off device, and skipped Orin-only tests report a named SKIP outside Orin.

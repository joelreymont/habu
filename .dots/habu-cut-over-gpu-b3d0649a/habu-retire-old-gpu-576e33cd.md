---
title: Retire old GPU emitters
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:42.508771+02:00"
blocks:
  - habu-promote-staged-gpu-2c5c3e97
---

Full context: after production coverage reaches zero unsupported capabilities, make staged GPU compilation default and delete string-first cg operation emitters plus old opt-ir from production; keep useful differential fixtures. Acceptance: design section 21 GPU/trust exits, Maki/PTX/device gates, and baseline requirements pass; no text-reparse or silent fallback path remains.

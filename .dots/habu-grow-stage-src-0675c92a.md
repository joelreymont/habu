---
title: Grow stage source capacity
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:26:19.480146+02:00\""
---

Full context: src/habu/stage2.f S2-SOURCE-CAP and src/habu/maker.f MK-SOURCE-CAP are fixed at 0x100000. Owner AOT validator growth passes certification/fixpoint but stage generation fails closed with stage2: source exceeds buffer, wrapper rc67/-2802 before artifact emission. Grow both synchronized source caps, add retired-edge and new-boundary regressions for stage2 and maker, and document the measured combined source watermark. Dependency: blocks habu-owner-seal-persist-1f23e205.

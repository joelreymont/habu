---
title: TMA dense emitter + mbarrier protocol
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T13:19:18.298584+02:00"
---

docs/tma-gather.md missing-piece-2. cp.async.bulk.tensor.{1,2}d loads into SMEM boxes, host-side descriptor build for static geometry via .param (cuTensorMapEncodeTiled mirror); mbarrier arrive/expect-tx completion as a checked collective protocol (same treatment as existing collective lowering). Stages shell carries over from cp.async 2B. Device-side tensormap.replace only for genuinely-runtime geometry with witnessed facts. After 'TMA legality design-rule family' and 'sm_121a process target row'.

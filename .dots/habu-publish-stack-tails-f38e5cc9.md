---
title: Publish stack tails with indexed stores
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T14:46:50.031997+03:00"
---

Measured donor FC:RUN publishes three already-register-resident cells as STP[x19], STR[x19+16], ADDx19,#24:12 code bytes. Postindexed STP#16 plus postindexed STR#8 can express the same cell writes/final data-stack pointer in8bytes, advancing only over initialized cells. Verify the responsible existing tail stack-move plan, physical register exclusions, ordering/barrier and immediate-range rules before editing; use exact compiler facts, no emitted instruction-pattern deletion. Extend existing relevant real native tail/stack E2E before implementation only for missing distinct behavior. Keep stack ABI, aliasing, spills and exceptions unchanged. Require same-source executable output and actual call-site code/instruction reduction, independent Astra review, matching full native and convergence. This is a bounded measured instruction reduction within data-stack93fe1f36, not a reduction in all memory residency or the historical25percent claim.

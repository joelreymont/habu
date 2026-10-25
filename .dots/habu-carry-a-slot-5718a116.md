---
title: "Carry a slot's stored name across edges on x86-64"
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T08:43:30.933215+01:00"
---

c7bf381a fixed select.f's residency pass: each slot keeps an edge name and a stored name, and the stored name crosses edges unchanged, as regalloc-verify tracks it (rows SLOT-* in test/compiler/native-loop.f). select-x64.f has the same DXLATE/DENTRY-VALUE? code (about lines 1807-1837) unfixed, so x86-64 tier 1 likely refuses the same shapes. Reproduce by cross-building the SLOT-* shapes for linux-x86-64 (a refusal happens at compile time, no x86-64 host needed), then fix at the same layer. Reported by tim, 2026-10-25. Claim: unassigned.

---
title: "Cut a warm build's cost"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T22:36:22.404001+02:00"
---

Problem: a warm engine build that compiles nothing still takes about 1.8 s wall. One measured cause is not fixed: every build runs the kernel build (tools/hb-build-core.f:315 KERNEL-BUILD calls LINKER:WITH each time, about 0.5 s). Kernel-object reuse is designed in .jj-ws.noindex/carl-layout/.out/design-kernel-reuse.md: the object keeps a fixed name under the hb stamp directory and carries the stamps of every file the process loaded; LINKER:REUSE-KERNEL skips the kernel build when every stamp still matches, and a bad object answers false instead of dying. The design waits on Joel's approval. Where the rest of the 1.8 s goes has not been profiled since the timestamps stack landed.

Acceptance: profile the warm engine build; root-cause the largest cost and remove it, starting with the kernel build (no compression without Joel's approval: a compact row encoding is not compression); a warm build equals a cold one.

Depends: Joel's approval of the kernel-reuse design.

Claim: unassigned.

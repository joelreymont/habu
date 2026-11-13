---
title: Let a build move where DATA starts
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:54:42.728507+02:00"
---

Problem: src/arch/arm64/link-kernel.f READ refuses a kernel object whose DATA-START is not the running hb's, staged or not, because src/habu/habu2.f ROUTE-HEADER routes header stores by the host's own DATA start (docs/bootstrap.md:99-102). So no change can delete a layout band below DATA (the dead JIT-SNAP band, the evaluate frame's $140-$157 gap). Acceptance: ROUTE-HEADER routes by the target's DATA start, READ's DATA-START refusal is gone, the bootstrap.md sentence goes, and a change that deletes JIT-SNAP lands through one stage. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-dstart

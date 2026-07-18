---
title: Ratchet measured engine size
status: active
priority: 2
issue-type: task
created-at: "2026-07-18T10:24:36.714541+02:00"
---

Own test/gate-build-size.f, docs/size-rca.md, and the relevant concise LESSONS.md additions only. Record the measured macOS candidate file size 148855 and __text 126916, previous feature baseline 165367/__text 132392, master __text 132576, primitives/base 18044 to 12708, primitives/extra 784 to 528, primitives/protect plus 68, seed dictionary plus 96, and the 16512-byte Mach-O page-floor crossing. Lower only the exact macOS ratchet to 148855; leave Linux unchanged pending exact Orin measurement. Acceptance: size gate with exact candidate, docs internally consistent, no unsupported ceiling claim.

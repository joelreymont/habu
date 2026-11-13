---
title: Read the kernel's TEXT-VA and DATA-VA once per object
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.060270+02:00"
closed-at: "2026-11-13T06:36:30.081659+02:00"
---

Problem: src/arch/arm64/link-kernel.f SYM (:196) scans the kernel object's symbols linearly, and TEXT-VA and DATA-VA called it once per linked site: 219,917 calls and 2,453,860 symbols visited per warm engine build, most of KOBJ@'s 2.78M calls. Acceptance: TEXT-VA and DATA-VA cost no scan per linked site; warm engine build time measured before and after; warm equals cold. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ksym.

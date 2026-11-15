---
title: Grow generated-source buffers
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:21.834468+01:00"
closed-at: "2026-11-16T04:07:35.980996+01:00"
---

Problem: src/lib/codegen.f CODEGEN:BUFFER has a fixed cap (E-CG-CAP) and rejects negative decimals (E-CG-VALUE); FFI declarations end in a raw -4700 on long names. Acceptance: buffers grow, both errors deleted, a 300-byte FFI symbol and callback name load. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

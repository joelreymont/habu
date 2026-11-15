---
title: Bound the memory grown token buffers keep
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T10:48:35.268842+01:00"
---

Problem: src/core/checker.f TOKBUF-ENSURE rounds a need up to the next 64 KiB and TOKBUF-GROW keeps all four old buffers mapped, because holders keep addresses into them across a grow (NAME-TOK's NMA, read by render.f and check-walk.f; DOES-NAME! passing TKF to CLAUSE-NAME, which ensures again). Needs that climb past each cap keep n(n-1) x 128 KiB for a final cap of n x 64 KiB: about 3.1 GiB at 10 MiB. Acceptance: the memory the token buffers keep mapped is bounded by a constant multiple of the largest need; a check whose needs climb to 10 MiB stays within it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-tokcap.

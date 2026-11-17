---
title: "Drop the loader's range-owner checks"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:04:06.690730+01:00"
---

Problem: src/core/include.f RUN-RANGE (:2371-2377) and CHECK-RANGE (:2326-2327) throw INCLUDE-RANGE-RC (-2110) when RANGE-OWNER is unset, differs from TOP@, or the frame's range is out of bounds; RUN-RANGE is private and runs only from CALL-UNIT inside LOAD-UNIT, which sets RANGE-OWNER to TOP@ first, and PUSH/POP save and restore it around nested loads, so none can fire: runtime checks (AGENTS.md: none). -2110 is also E-FS-TIMEOUT, so a throw would print the timeout text. Acceptance: INCLUDE-RANGE-RC, CHECK-RANGE, RANGE-OWNER and FRAME-OWNER's save and restore are deleted, with RUN-RANGE's CHECK-ACTIVE call if TOP@ is always set there; the engine converges. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rng.

---
title: Fix callback reuse E-NCOMP-STATE in lane 11
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T16:10:03.223537+02:00"
---

Problem: test/ffi-callback-file-cache.f, the callback FILE image build exits 67 with uncaught throw -8570 (E-NCOMP-STATE; src/lib/ffi-callback.f ARM-STUB/STUB-ENTRY read a 0 dispatch cell) after files reused 1. Lane 11 (lqwqmsnq bf1b129f) fails it singly on its native-build.f candidate; master 7a3acd4c candidate (carl-base) passes it. Acceptance: root cause at the responsible layer, fixed as a fold-in into lqwqmsnq; the file rc 0 on a rebuilt candidate; the row it belongs to rc 0. Depends: none. Ownership: lane 11 code the cause names. Verify: bin/hb --load test/ffi-callback-file-cache.f. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-rcb.

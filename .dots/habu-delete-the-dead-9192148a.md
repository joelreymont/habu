---
title: Delete the dead top-level hook
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:31:34.762531+01:00"
---

Problem: set-top-check writes TOP-HOOK-CELL, which only top-check@ reads, and nothing on arm64 calls top-check@ (src/habu/prims.f, src/habu/habu1.f BTOPCHECKFETCH and set-top-check's builder, src/habu/layout.f TOP-HOOK-CELL, src/core/top-row.f TR-INSTALL); tools/checked-boundary-lint-core.f and tools/hook-sites.f, run by tools/check-core.f CHK-RUN-BOUNDARY, guard only that install. Acceptance: the two primitives, the cell, TR-INSTALL, the boundary lint, its check step and hook-sites.f are gone, rg -i --no-ignore finds them nowhere outside x86-64 files; the x86-64 kernel's copies and test/x86-64-kernel-engine.f wait for the x86-64 step. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w131.

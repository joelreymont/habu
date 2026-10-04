---
title: "Check the hb-build driver's definitions"
status: open
priority: 3
issue-type: task
created-at: "2026-10-04T06:17:00.423562+03:00"
---

Problem: tools/checked-boundary-lint.f run over all of src was reported as 312 UNCHECKED-DEFINITION findings in files nobody touched (build.f, bundle-argv.f, cell-grid.f, code-bytes.f, code-origin*.f, ... prim-ref.f; the same on B3). Measured on master 9d126e86 (221 src .f files, sorted, 30 per invocation for ARGV-MAX 64): 166 findings, all in the invocation holding src/habu/build.f and only in build.f and the files after it (code-origin-x64.f 37, crash.f 30, code-origin.f 25, data-claims.f 14, debug-watch.f 12, cell-grid.f 12, build.f 11, ...). The lint carries a top-level `0 set-check` into the later files of one invocation (UB-CARRY-OFF, tools/checked-boundary-lint-core.f:119-121 and :461), modelling one load sequence; a sorted file list is not a load order. Linted alone, src/habu/crash.f, code-origin.f, boot-x64.f and prim-ref.f report 0. The one real site is src/habu/build.f: `0 set-check` at :14, never restored, leaves its 11 driver definitions unchecked; its comment says the window "dissolves with staged fixpoint source checking: habu-staged-fixpoint-src-0b5fc6e6", a dot in neither .dots nor its archive. Acceptance: build.f's definitions certify under the hook and the `0 set-check` and its dangling reference go; if a definition cannot certify, its refusal and probe are recorded here first. Files: src/habu/build.f. Verify: `bin/hb tools/checked-boundary-lint.f src/habu/build.f` rc 0 with no finding; the tools/hb-build-*-test.f rows. Depends: none. Ownership: src/habu/build.f. Claim: unassigned.

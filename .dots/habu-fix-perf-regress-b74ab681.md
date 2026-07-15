---
title: Fix perf-regress fork-worker registry contamination
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T17:22:10.042452+02:00\""
---

Hit by TWO lanes now (effseal 2026-07-15 + autoparity same day; LESSONS 1256 records the class): the inprocess stdlib/lint-libs/ptx-toolchain group intermittently fails at tools/ptx/perf-regress.f with 'malformed registry row: ... device isolation fixture' - a dev-25w/test-dev FIXTURE row from tools/ptx/perf-compare-test.f's in-memory PERF:ADD-LINE appearing in perf-regress's parse, under machine saturation / specific harness invocations (autoparity reproduced it twice with 'test/run.f -- --under bin/hb' while a fresh-root plain run.f on the same tree is green; standalone perf-regress is always green rows=40). Root-cause candidates: the fork worker inherits the parent's PERF ROWS arena / SB state copy-on-write and perf-regress's PERF:LOAD->RESET does not clear whatever LAST-LINE/LOK buffer the error path reads, or the fork-order race lets compare-test's arena survive into the regress fork. Fix: make the perf registry state hermetic per fork (explicit full RESET incl. line/error buffers at PERF:LOAD entry; or isolate the two tests into different forks with a fresh image), plus a regression that runs compare-test then regress in ONE fork deterministically. Files: tools/ptx/perf-registry.f (RESET/LOAD/LAST-LINE), perf-compare-test.f, perf-regress.f, the gate group wiring. Verify: the deterministic repro fixture red-first; then the inprocess group green under repetition (run the group 20x). Ownership: ptx perf tooling.

Claim: agent=perfrace workspace=.jj-ws/fable-perfrace

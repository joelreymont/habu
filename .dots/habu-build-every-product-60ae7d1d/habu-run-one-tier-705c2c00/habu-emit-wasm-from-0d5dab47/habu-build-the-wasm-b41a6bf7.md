---
title: "Build the Wasm test's passing programs once and size the browser tests' large file plainly"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T19:18:46.624070+02:00"
closed-at: "2026-11-10T20:15:33.304278+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem (feature review B, F3 and F4): test/wasm/hb-build.f starts a build process per case (21), where the command line and exit status need one success and the refusals; the programs that run to status 0 with no output compared (k, d, m, execute, catch, grow) can be one source and one build. test/browser/echo-test.f LARGE-CASE and test/browser/storage-test.f PAYLOAD! size their file as WPROF:DATA-BASE + the module file's size + a page + 13 and say it is larger than the module's memory or image; a linked module's file holds only non-zero runs, so that is false. Acceptance: one build for the status-0 programs, wall and CPU time of the test before and after recorded here; the two browser tests size the file as a plain number of pages plus 13 and say a file of several pages. Files: test/wasm/hb-build.f, test/browser/echo-test.f, test/browser/storage-test.f. Verify: the three tests end test: ok. Depends: the refusals dot and the data-segments dot (same file). Ownership: those files. Claim: unassigned.

Measured (test/wasm/hb-build.f on the stack's engine, a loaded machine, so CPU is the comparable number): before, 26 builds, 88.2 s user and 2.8 s system, wall 141 s; after, 21 builds, 74.5 s user and 2.5 s system, wall 247 s under a load average near 115.

---
title: build-profile takes an option-shaped argument as a path
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T21:48:57.094012+02:00"
---

Problem (the fix re-review of jerry-w4, 2026-11-10, read from the code): tools/build-profile.f ARGS-OK? (:35) only counts the operands, so a misplaced option is taken as a path: '-- --engine --target wasm /tmp/e' builds a host hb into ./--target and truncates ./wasm and /tmp/e for the reports; '-- --engine --target wasm app.f' truncates app.f for the JSON report (REDIRECT opens with O_TRUNC); '-- --repl app.f t j' takes --repl as the source. A missing source runs the kernel build before the build throws, where tools/hb-build-lib.f (:210) refuses it up front with status 66. Acceptance: a line whose first operand starts with '-' and is not --engine, or any of whose three output operands starts with '-', is the usage message with status 64 before any build, profile or file; a source that is no file ends with 'build-profile: no such source' and status 66 before any build, profile or file; an accepted line builds and reports as before. Files: tools/build-profile.f. Verify: each refused line run, its status, and no file made. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pa.

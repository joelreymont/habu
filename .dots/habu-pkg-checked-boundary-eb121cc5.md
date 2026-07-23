---
title: Package checked-boundary lint test library
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-22T15:48:08.449091+02:00\""
---

Why: tools/checked-boundary-lint-test-lib.f defines its test state and helpers globally, so any body edit fails the package ownership gate. The rejected candidate 87c155e7 proved the package migration but used a four-word owner, retained dead timeout and error-buffer state, and asserted an empty manufactured error span instead of the actual scalar result.

Owner and files: package CBLT in tools/checked-boundary-lint-test-lib.f, plus the sole qualified MAIN call in tools/checked-boundary-lint-test.f. Keep every fixture path, buffer, helper, and test private; publish exactly MAIN ( -- ). Rename each CBLT-* global to its short package-local tail. Remove TIMEOUT-MS and the unused 4096-byte ERR buffer. CORE-FINISH continues returning `( -- n n outcome )`; clean and failing assertions must prove the returned error length is the scalar zero value rather than indexing a fake buffer. Preserve generated fixture bytes, output buffering, finding text, exit codes, cleanup, and all current cases. No aliases, public test bridge, copied validator, new timeout, or compatibility globals.

Acceptance: the production checked-boundary suite executes CBLT:MAIN and passes; every clean and failing case still reaches the real checked-boundary core path; hostile body mutation inside the packaged library passes package ownership while removing the package owner fails with the exact measured findings; exact typed-local and package diff gates pass; no CBLT-* global remains; package CBLT exposes only MAIN; host-lint and filemap-lint pass. Base is verified master@origin 2db115be. Claim: agent=cblt_fix workspace=.jj-ws/habu-pkg-checked-boundary-eb121cc5.

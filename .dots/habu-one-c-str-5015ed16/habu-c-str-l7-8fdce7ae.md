---
title: "C-string L7 tools B: open report, lint and bundle paths through FS"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T00:24:17.499580+02:00"
---

Problem: tools/aot-call-report-lib.f:93-104, aot-call-report-test.f:30,94, gate-json-assert-core.f:73, lint/text.f:71, repl-lint-core.f:88, diag-to-sarif-core.f:382 and bundle-lib-core.f:116-134,278,337-348 each keep a private C-path copier, buffer and cap that write a terminator and never refuse a 0 byte (census C29, C5). Acceptance: copiers, buffers and caps deleted; opens, stats, writes and renames go through FS:>PATHZ, FS:EXISTS?, FS:TRY-STAT-MODE, FS:WRITE-ALL and RENAME-FILE; bundle-lib-core.f requires fs.f and fs-mutate.f (design 4E C5: nothing in the recovery route loads it); the tool tests, lints and gate consumers green; check.f clean. Files: those seven. Verify: carl-dots/.out/carl-file/brief-l7.md proof list. Depends: habu-c-str-l3-0c9f0875. Ownership: the seven files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-cstr-l7

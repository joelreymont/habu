---
title: Expand required files for a stdin check
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T04:12:40.688003+02:00"
closed-at: "2026-10-15T01:13:03.530584+03:00"
close-reason: "Completed by master ancestor 530695d4: stdin and CHECK:SOURCE both use CHK-EXPAND-BYTES; required-type file/stdin/source-buffer parity is covered in check-test-lib. Current implementation and recorded qualification verified."
---

Problem: tools/check.f reading its source from stdin drops the definers that required files supply: CHK-MATERIALIZE-STDIN (tools/check-core.f:601-605) writes the bytes to the source path but never runs CHK-EXPAND-RESET and CHK-EXPAND-PATH, which CHK-MATERIALIZE-FILE (:613-619) runs for a path, so a source that requires a definer library checks differently from stdin than by path. Acceptance: one source gives the same verdict and diagnostics by path and from stdin, including words and definers from required files; a case through tools/check-test-lib.f written before the code fails first. Files: tools/check-core.f, tools/check-test-lib.f. Verify: tools/check-test.f. Depends: habu-let-check-f-f02aa703 (r4-render, plan 15). Ownership: stdin materialization.

Owner: viper (zed-habu checker plan): CHECK:VERIFY-BYTES expands the closure from bytes plus a path, and check.f's stdin path will share that materialization. Not round-4 work.

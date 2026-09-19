---
title: check.f fails open on prefix paths
status: active
priority: 2
issue-type: task
created-at: "2026-08-20T12:11:31.997328+02:00"
---

Claim: alder, .jj-ws/alder-check-prefix, base b4efad25. The original CLI
probe still returns zero with no diagnostics. Restrict this change to
source-list selection and its real-entry tests; no checker/compiler edits.

Implementation: reject a source list with rc64 when every explicit path is
already provided according to ENTRY-RESOLVE. Resident verification would
skip every such input; engine-provided paths also skip in the spawned child.
Mixed provided/new lists still check the new files. Source, stdin and
single-file selection are unchanged. The diagnostic names the skipped-work
condition rather than reporting successful verification.

Proof: actual CLI regressions cover a prefix file, normalized alias under
JSON/all-errors, and mixed lists containing valid and invalid new sources.
The old core fails four assertions (both refusals returned zero); final
check-cli-boundary passes. The existing lib/test.f success case now starts
a fresh tool process so its resident phases actually verify that file.
Astra review and its wording follow-up are clear. Private b4efad25 tree/host;
Hazel supplies the final integration/gate and dot closure.

Found by route3-1 (2026-08-20): pointing tools/check.f --source-list at a canonical prefix path (src/core/type-schema.f) exits 0 in 0.6s HAVING CHECKED NOTHING - REQUIRE-KNOWN? (include.f:100) byte-compares, the boot prefix marks its files provided, CHK-DEP-PRELOAD? (check-core.f:1046) silently skips. The ./-prefixed spelling of the same bytes is checked for real. A gate tool exiting 0 having verified zero definitions is fail-OPEN. Fix: CHK-DEP-PRELOAD? refuses by name when every positional is engine-provided. LESSONS.md:784 only half-records this (expects a noisy E-UNDEFINED; reality is silence).

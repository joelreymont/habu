---
title: "Name the duplicate check.f's preverify refuses"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T08:52:24.884384+02:00"
---

Problem: 'bin/hb --load tools/check.f -- tools/hb-build-lib.f' (also '-- --json-errors tools/hb-build-test.f') exits 78 = E-DUP-DEFINITION (src/core/layout-buffer.f:28) with only 'check.f: source preverify failed before run / label / throw code 78' and an empty diagnostic buffer (tools/check-core.f CHK-PREVERIFY-FAIL), while 'bin/hb --load tools/hb-build-test.f' loads and runs. Measured on 53e02ad8 with rb3/g1 (lead rerun) and by the r4-hbbinst lane. Either preverify sees a duplicate the real load does not, or the duplicate throw renders no diagnostic: both are defects. Acceptance: reproduce on the integration tip after the r4-expand merge lands (it rewrites check-core.f and ends an all-errors check at a duplicate) and say whether it still occurs; check.f on the hb-build files passes, or refuses naming the duplicate and both definition sites; any duplicate throw on the preverify path renders its diagnostic; a check-test case seen failing first. Base: after the merge (dots 02fabb03 ... 2eb1290e) lands.

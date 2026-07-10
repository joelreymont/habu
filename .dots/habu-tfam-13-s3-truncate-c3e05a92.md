---
title: "TFAM 13 S3: verify truncated-declaration collector post-refactor"
status: open
priority: 3
issue-type: task
created-at: "2026-07-10T12:00:00.000000+02:00"
---

Destruction-review finding S3 from dot habu-tfaam-13-adt-5d3288f0 (S2 audit,
2026-07-10). The original finding cited CA-ADD-SUPPORT-TRIPLE / CA-ADD-SUPPORT-SUM
in tools/check-all-errors-core.f silently skipping an EOF-truncated
TYPEFAMILY/unterminated SUMTYPE (no support row, no diagnostic). Those words no
longer exist on fable: the support collector was refactored to
CHECK-ALL-ERRORS-SUPPORT-RESET / CHECK-ALL-ERRORS-SUPPORT+ (check-all-errors-core.f
:178-181). Rediscover whether the truncated-declaration silent-skip still exists
against the refactored collector. If it does, the collector must report the
truncated declaration (declaration packet parity, §24); add a red-first fixture.
If the refactor already closed it (e.g. the S2 CHECKER-DEFSUM-NOEND routing now
feeds a support row), close this dot with the proof.

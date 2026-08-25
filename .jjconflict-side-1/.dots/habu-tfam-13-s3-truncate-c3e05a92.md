---
title: "TFAM 13 S3: verify truncated-declaration collector post-refactor"
status: closed
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

## CLOSED — the silent skip is GONE; regressions pinned (2026-07-11)

Rediscovery against the refactored collector, empirical fixtures through the
live check paths:
- Unterminated SUMTYPE at EOF (`SUMTYPE truncsum 1 / VARIANT one a ;VARIANT`,
  no ;SUMTYPE): exit 70 + the E-BAD-DECLARATION packet, reason `missing
  ;SUMTYPE` (json mode shows the full declaration packet; §24 parity). Bare
  `SUMTYPE x 1` header at EOF and an unterminated VARIANT arm produce the same
  packet. The routing is CHK-SUM-REGISTER -> CHK-BLOCK-COLLECT unterminated ->
  CHECKER-DEFSUM-NOEND (check-core.f:756-770) — the S2 NOEND wiring feeds the
  packet, exactly the hypothesis in this dot.
- `TYPEFAMILY x 2` at EOF is NOT a truncation — TYPEFAMILY is a complete
  one-line declaration (docs 9.1, no terminator). The truncatable header form
  (missing arity) reports `missing arity` through the same packet, exit 70.
- NO silent skip exists on either the plain, json, or ALL-ERRORS paths.

Observed (in-scope note, not a skip): ANY bad declaration is file-fatal — a
body error elsewhere in the same file is not also collected under --all-errors
(a broken family would poison downstream checking; the declaration packet is
still emitted and the run fails closed).

Regressions added (tools/check-test-lib.f): CKT-DIRECT-ALL-JSON-STDIN runner +
CKT-TEST-SUM-NOEND-ALL + CKT-TEST-TFAM-NOARITY-ALL — the truncated forms
through the ALL-ERRORS collector path asserting exit 70 + packet + reason
(previously only the plain json path was pinned).

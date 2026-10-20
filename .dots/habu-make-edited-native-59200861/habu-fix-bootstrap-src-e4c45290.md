---
title: Fix bootstrap source producer flag
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T17:17:08.413459+03:00"
---

The first periodic no-binary recovery run at frozen 778db91a fails before its ABI assertion: generated probe exits70 with stderr exactly false, empty stdout. source-producer.f COMPILER-CHANGED uses false before the Gforth/native cold prefix loads prelude.f. Keep the early core dependency boundary and use an equivalent core boolean expression, without moving the whole prelude or relaxing the ABI probe. Existing bootstrap-engine-stack.fs is the observed regression. Run focused probe then the check-only recovery chain after the fix; retain exact source/command/exit and independently review the bounded source correction before core landing. Evidence: ~/.cache/tmp/habu-bootstrap-check.e43Wep/bootstrap.log. No public engine or selector change.

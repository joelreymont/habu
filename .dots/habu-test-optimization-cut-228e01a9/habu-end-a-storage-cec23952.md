---
title: End a storage name and first type token at the line
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T14:12:40.492260+02:00"
---

Problem (fold 327 r4-scheme, bc29cde3): after a storage definer (TYPED-VARIABLE, TYPED-BUFFER, DYNAMIC-BUFFER, LAYOUT-BUFFER) the name and the first type token still cross lines on both paths: 'TYPED-VARIABLE X' followed by 'n' on the next line is accepted (src/core/layout-buffer.f STORAGE-NEXT-TOK, src/habu/verify-source.f SCAN-STORAGE-TOK), though the rest of the spelling now ends at its line (CHECKER-TYPE-SPAN-BREAK?). The gate's first type token also skips comments while the load path does not. Acceptance: a definer whose name or first type token is not on its own line is refused as a missing name/type with the same reason and place on the load path and through tools/check.f, and the next line is left for the next statement; the gate and load agree on comments before the type; cases seen failing first in test/c2-memory-scope-refusals.f and test/gate-diagnostics-lib.f. Files: src/core/layout-buffer.f, src/habu/verify-source.f.

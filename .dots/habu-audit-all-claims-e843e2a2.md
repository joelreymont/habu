---
title: Audit all ✓ claims in cl-symbols.md
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:26:45.236906+02:00"
---

Files: docs/cl-symbols.md, src/**/*.zig, stdlib.habu
For each symbol marked ✓, verify implementation exists:
- Grep source for function/variable
- Check if stub vs real implementation
- Mark false claims in audit file
Create /tmp/cl-symbols-audit.txt with findings.
Verify: Complete list of verified vs false claims.
Est: 60min

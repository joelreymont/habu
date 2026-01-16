---
title: Audit error masking patterns
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:14:59.009600+02:00"
---

Files: entire src/ codebase, create /tmp/error-audit.txt
Run rg for each pattern:
- rg 'catch unreachable' src/ > /tmp/error-audit.txt
- rg 'catch return(?\! error)' src/ >> /tmp/error-audit.txt
- rg 'catch return null' src/ >> /tmp/error-audit.txt
- rg 'catch \|_\| return' src/ >> /tmp/error-audit.txt
- rg 'catch blk:' src/ >> /tmp/error-audit.txt
- rg 'orelse unreachable' src/ >> /tmp/error-audit.txt
Count instances per file. Create triage plan.
Verification: audit file exists with categorized instances

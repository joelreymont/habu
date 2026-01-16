---
title: Triage error masking instances
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:04.776354+02:00"
---

Files: /tmp/error-audit.txt, create /tmp/error-triage.md
Review each instance from audit:
- Mark as KEEP (truly impossible, add // unreachable: proof comment)
- Mark as PROPAGATE (change to try, update signature)
- Mark as HANDLE (needs proper error handling)
Group by file. Prioritize critical paths.
Dependencies: habu-audit-err-masking-233d9635
Verification: triage file categorizes all instances

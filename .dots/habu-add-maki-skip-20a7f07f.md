---
title: Add maki skip to stale-status-lint
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:57:38.055276+02:00"
---

tools/stale-status-lint.f:441 walks SS-ROOT (default s" .") and :194 SS-ALLOWED? whitelists ONLY STATUS.md and LESSONS.md; :199 SS-SKIP-PATH? skips only .jj-ws/. A maki/STATUS.md (full of self-check counts) and any count-bearing maki/*.md fail the gate. PLAN.md says maki carries its own maki/STATUS.md, so this WILL break the gate when maki lands.
- Files: tools/stale-status-lint.f:194-200 (extend SS-SKIP-PATH? to skip maki/, or SS-ALLOWED? to allow maki/STATUS.md), audited + with a test.
- Verify: a maki/STATUS.md with counts no longer flagged; root STATUS.md rules unchanged; a count-bearing maki/foo.md handled per chosen policy.
- Dep: relevant once maki/ scaffold exists. Note: this edit is in a habu core lint, a deliberate fence-maintenance exception.

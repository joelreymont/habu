---
title: Fix the recovery stage fixpoint crash
status: open
priority: 1
issue-type: task
created-at: "2026-10-15T19:11:20.732061+03:00"
---

After c158e3ed removes the early false dependency, the direct Gforth stack gate passes and check-only recovery reaches hb-stage, then crashes with exit 134 / signal 0a during its first fixpoint loop, before stage2-got. The original early source-producer failure is resolved and is a distinct task. Exact recovery artifacts are held in /Users/joel/.cache/tmp/habu-bootstrap-check.producer.q7Hsgb; the handoff is /Users/joel/.cache/tmp/carl-bootstrap-producer-handoff.md. Carl owns a bounded diagnosis in .jj-ws/carl-recovery-check. Reduce the retained native stage failure before repeating the full recovery. Acceptance is the real recovery path with no public binary substitution and independent review of any substantive fix.

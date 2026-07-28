---
title: Remove FILEMAP process references
status: active
priority: 2
issue-type: task
created-at: "2026-07-28T10:47:08.235402+02:00"
---

Why: after the manual index and its gate disappear, live guidance must not tell agents or plans to maintain or run them. Owned result: remove FILEMAP.md and filemap-lint requirements from AGENTS.md, STATUS.md, LESSONS.md, PLAN.md, MODEL-CAD-V2-PLAN.md, docs/model-unified.md, docs/compiler-ir-design.md, docs/porting.md, and docs/worker-briefing.md. Preserve every remaining gate, source-list rule, lesson, plan dependency, and substantive design statement; rewrite grammar only where needed. Closed historical dots and docs/archive are frozen history and are outside this leaf. Forbidden: deleting other docs, changing runtime/code, weakening host/dot/type gates, rewriting unrelated lessons, or creating a replacement inventory. Checkpoint: an exact path census names every live non-dot, non-archive reference before edits. Acceptance: the same census reports zero live references after edits; AGENTS master workflow remains complete without filemap; STATUS and plans name only real gates and files; Markdown links and package names remain valid. Ownership: live process and plan prose only.

Claim: agent=codex-filemap-docs workspace=.jj-ws/habu-remove-filemap-process-f72075cc

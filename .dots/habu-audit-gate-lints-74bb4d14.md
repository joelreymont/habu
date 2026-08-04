---
title: Audit gate lints for untracked-file scope leaks
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T10:06:26.446133+02:00"
---

Why: a gate tool that walks the ambient filesystem instead of the tracked tree can produce different verdicts for the same commit in different workspaces. Behavior: audit only the gate tools present in the exact tree and registered on the master gate list or invoked by the exact `test/run.f` lint-tools phases. For each, either prove its enumeration is tracked-only by construction or add an explicit untracked-path skip with a hostile fixture. Record the per-tool verdict table in the dot on closure. Owner: the present `tools/` lint family; no new package. Acceptance: a written per-tool table naming the tool, enumeration mechanism, and tracked-only proof or added skip; every added skip has a hostile fixture proving an untracked file cannot sway the real `bin/hb --load` path. Verify: the current full master gate list is green with byte-identical verdicts from the main workspace and a fresh `.jj-ws` workspace on the same commit.

---
title: one gate command and one checker command
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.031255+02:00"
---

Problem: LLM.md:45-47 spells the gate as a 20-file --load list, skills/habu-gate/SKILL.md:18 and docs/bootstrap.md:236 as 'bin/hb --load test/run.f' (test/run.f:7-9 requires its own deps); LLM.md:27 lists 24 preload files before tools/check.f which tools/check.f:4-25 requires itself and already omits tools/reserved-name-lint-core.f (drift visible); LLM.md:84-110 describes a retired harness scorecard; four overlapping agent protocols (CLAUDE.md, LLM.md, docs/worker-briefing.md, forth.md preamble). Acceptance: the short forms everywhere; LLM.md folded into CLAUDE.md (uppercase-words and TRUST-audit rules kept) and deleted; docs/worker-briefing.md deleted (dead path /home/joel/Work/Habu, wrong constant GB-SIZE-BASELINE-LINUX, 0 references) with its four unique rules moved to CLAUDE.md. Files: LLM.md, CLAUDE.md, docs/worker-briefing.md, docs/bootstrap.md. Verify: rg finds one spelling of each command. Depends: none. Ownership: docs. Claim: unassigned.

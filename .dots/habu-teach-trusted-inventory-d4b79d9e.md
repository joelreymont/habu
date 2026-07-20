---
title: Teach trusted-inventory DOT-EXISTS? epic-nested dot paths
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-20T13:04:58.901227+02:00\""
---

Tooling gap found by the MATCH-pin lane (bd23829a): tools/trusted-inventory.f DOT-EXISTS? resolves an owning dot id only as a TOP-LEVEL .dots/<id>.md file, so a TRUSTED.md row whose owner dot lives in an epic subdirectory (.dots/<epic>/<id>.md - where most new dots now live) fails strict validation as 'missing owning dot'. The lane worked around by designing its fixture to need no trusted rows; the relocmeta landing hit the adjacent version (rows pointing at a CLOSED dot) separately. Fix: DOT-EXISTS? searches recursively (the same resolution 'dot show' uses - any .dots/**/<id>.md with status not closed), with a fixture proving an epic-nested owner validates and a genuinely-missing id still fails. Territory: tools/trusted-inventory.f + its test.

Claim: agent=tinv workspace=.jj-ws/fable-tinv machine=spark (owns tools/trusted-inventory.f + its test)

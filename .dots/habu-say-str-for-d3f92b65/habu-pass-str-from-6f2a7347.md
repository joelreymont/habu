---
title: Pass str from unmarked test programs
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:11:10.842242+01:00"
---

Problem: the str row commits leave 28 suites red: program text with no split-str line (evaluated, generated, fixtures, REPL lines) still passes ^u8 n to words now taking str (census /tmp/jerry-str/census-fo.txt). Acceptance: the full suite passes on the landing stack. Depends: none. Claim: agent=jerry lanes=.jj-ws.noindex/jerry-foa..jerry-foe

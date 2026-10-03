---
title: Locate a refused using at its source line
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:14:00.382683+02:00"
---

Problem: tools/check.f reports a refusal raised by a top-level 'using' at a line of its generated run script, not at the source. Measured on 6f7186aa with rb4/h1: a file whose line 1 is 'using ZQNOPE': 'bin/hb --load f.f' rc 91 'hb: using: unknown package: ZQNOPE at .../f.f:1'; 'bin/hb --load tools/check.f -- f.f' rc 91 '... at HB_TMP/habu-check-*/run.f:7'. 'using (' on line 4 the same (f.f:4 vs run.f:12). The comment-opener reading is consistent: 'package ( ... ;package' loads and checks rc 0, 'using (' is refused by both with rc 91 (docs/forth.md:1300 rule; walker CHK-WALK-USING tools/check-core.f:839 takes the next token as the name). Acceptance: check.f reports a refused 'using' (and any refusal raised by a statement it replays into run.f) at the source file and line where the statement sits, in prose and --json-errors; a check-test case seen failing first. Dot 87b406ce (duplicate STR-TAB at run.f:20, a resident source) is related: say whether one change covers both. Base: 6f7186aa or later.

---
title: Pass str from unmarked tests
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T15:30:13.522594+01:00"
closed-at: "2026-11-23T10:11:21.768178+01:00"
---

Problem: six test files without split-str call words whose rows now say str with ^u8 n (test/top-row-storage-test.f, compiler/width-type.f, deftype-suite.f, cast-negative-suite.f, split-str-test.f, type-decl-suite.f), so tools/check.f rejects them. Acceptance: each call passes a str (>str); check.f output on each file equals master's apart from paths. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-umt.

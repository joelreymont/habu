---
title: Say why = rejects two pointers of different types
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:19:40.321152+01:00"
---

Problem: ': R ( ^STORED:turn ^STORED:node -- bool ) = ;' is rejected with '`=` takes n n; the stack holds ^stored:turn ^stored:node' and the fix 'give `=` n n: convert or replace the value', though = also takes two pointers of one type (prims.f :377-387 pointer rows); the expected row is the first overload's while both failed. Acceptance: the rejection names what = takes for a pointer (two pointers of one type) and that turn and node differ; test/compiler/stored-type-param.f CASE-POINTER-KINDS pins it and fails first. Depends: none. Claim: unassigned.

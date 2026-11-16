---
title: Assert records without pinning the gutter
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T06:18:08.250663+01:00"
blocks:
  - habu-reject-a-ptr-683953b2
---

Problem: a record's note and caret lines are indented by its line number's digit count (a record at line 121 indents '= help:' by 4, at line 1201 by 5: /tmp/jerry-w188/v/g3.f, g4.f), but tests compare rendered lines with a fixed indent for records placed in the suite file itself (rg '= evaluated:' test: structure-decl-suite.f, shadowed-arity-test.f, decl-diag-capture.f, tail-parity.f, cast-negative-suite.f and others), so a suite that grows past 999 lines fails with no behaviour change. Acceptance: those assertions check the record's fields, or build the indent from the record's line, so adding lines to a suite breaks none of them. Depends: habu-reject-a-ptr-683953b2 (its lane edits these tests). Claim: unassigned.

---
title: Name the definition ncomp cannot compile
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T00:07:43.889671+02:00"
---

Problem: test/does-clause-record.f RUN-REJECTED (:157) loads `: DR-BAD ( n -- n ) dup create , DR-NO-SUCH-WORD does> ( -- n ) @ ;`. The hook refuses dr-bad by name, then src/compiler/native/compiler.f:672 REPORT-FAILURE prints "ncomp: cannot compile " with an empty name: NAME-BUF/NAME-U are not set on this path (/Users/joel/.cache/tmp/kestrel-r4-rev223/A-test-new.log line 4). Acceptance: find which definition the native compiler is refusing here (dr-bad or its ;does clause) and why its name is empty; the line names it, or no ncomp line prints for a definition the hook already refused, whichever is the responsible layer's contract; does-clause-record.f asserts the stderr. Files: src/compiler/native/compiler.f, test/does-clause-record.f. Verify: rebuild if baked, focused test.

---
title: The other four migrated corpora still build strings
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-19T00:14:25.059612+02:00\""
---

Claim: agent=destr-1 workspace=.jj-ws/habu-thecut

tools/codegen-compare-migrated{2,3,4,5}.f still hand a built s-literal to NMIGRATE:DEFINE per row; master 3e571921 gave migrate a stream entry (NMIGRATE:NEXT) and rewrote migrated.f's ten rows as top-level definitions. Convert the other four the same way - every row except any DEFINE-DATA rows (those wait on 77d34d82). They are KEEP files under the compare-harness deletion (2b07fd19 corrected keep list): four scheduled inventory suites walk them, so the conversion also de-strings what survives the delete. Mechanical-pass warning: bound edits by the last quote on the line and verify the quoted prefix byte-identical (LESSONS.md).

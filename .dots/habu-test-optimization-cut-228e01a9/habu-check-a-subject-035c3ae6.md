---
title: "Check a subject that starts with #!"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T10:03:25.762757+02:00"
closed-at: "2026-10-15T01:26:46.745460+03:00"
close-reason: "Completed or superseded on current master. potmxxrl handles leading shebang; file/stdin/source-list/location and later-#! cases are covered. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim."
---

Problem (review 257 of usingloc ab7fcef3): the engine skips a leading shebang line (src/habu/habu2.f:1397 C-SOURCE-SKIP-SHEBANG reads the source's first two bytes), so `bin/hb --load s.f` accepts a file whose line 1 is `#!/usr/bin/env hb`, but `bin/hb --load tools/check.f -- s.f` refuses it E-UNDEFINED: #!/usr/bin/env, rc 70: check.f's generated run.f places the subject after its own prelude, so the engine's skip never sees the subject's first bytes. Load and check disagree. Acceptance: a failing check-test case first; a subject whose first line starts with #! checks as it loads (rc 0 clean, its refusals reported at the subject's own line numbers) in prose, --json-errors, --source-list and stdin; a #! anywhere but line 1 is still an ordinary token. Fix at the layer that owns subject text (check-core.f's run builder, or the engine's skip if the subject's start is visible to it); say which and why.

---
title: Check a call to a refused word after a loader
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:20:28.242228+02:00"
---

Problem: tools/check-core.f (CHK-ALL-SEG-ACT comment ~:1790) and c7 5185360a's message say that under --all-errors a later segment checks a call against a refused definition's declared signature, so one refusal reports once. Review 246 measured otherwise on the merge head's engine: ': X ( -- n ) NOPE ;' then 's" b.f" required' then ': Y ( -- n ) X 1 + ;' under --all-errors prints two lines, the second 'E-UNDEFINED in y: undefined word X' (same in --source-list mode, and across two files with no cycle), while the same file without the loader prints one line. Same output on the pre-merge chain (r4-expand, r4-tbuf), so the claim never held across a segment boundary; TEST-REQUIRE-CASCADE only covers a call to a clean word. Leads (not isolated): the no-cascade record is the EFF-RECOVERY row (src/core/checker.f:16995-17001), honoured only while RECOVERY-ROW? holds (:7496-7499); MULTI-ERR-BEGIN sets the floor once per session (cell-effects.f:46). Probes: $HOME/.cache/tmp/kestrel-r4-rev246/casc/ (t1 no loader, t2 two files, t3 loader). Acceptance: a call in a later segment to a definition refused in an earlier one reports no second error, in prose, --json-errors and --source-list; or the comment and docs state the real behaviour if the refusal must cascade, with the reason; a check-test case seen failing first. Base: after the master merge (checker.f changed on both sides).

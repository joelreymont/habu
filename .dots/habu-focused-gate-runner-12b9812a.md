---
title: Focused gate-runner slices exit 77 with lone-colon output
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T20:39:00.798054+02:00"
---

printf '' | bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-libs-ptx (and -- lint-manifest) exits rc=77 printing a single ':' byte, on a PRISTINE fable tree (proven 2026-07-08 in .jj-ws/fable-red by jj-restoring all local edits and rerunning; HB_TMP set makes no difference). 77 matches tools/lint/token.f E-LINT-TOKEN-CAP (positive throw code) but no diagnostics surface. docs/forth.md and LESSONS.md document this exact command as the focused-slice entry, so either the entry regressed or it now requires pool/stats context the docs do not mention. RCA: find what throws 77 during GR-MAIN (GR-DISPATCH GSI-* include vs GR-STATS GS-* append), make the failure attribute itself (label, phase, rc name) per docs/forth.md boundary-spawn rule, fix the entry or the docs. Full suite path bin/hb --load test/run.f is unaffected.

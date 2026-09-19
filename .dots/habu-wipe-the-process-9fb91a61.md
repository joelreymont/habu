---
title: Wipe the process-command staging buffers on request
status: active
priority: 2
issue-type: task
created-at: "2026-09-19T23:39:51.314818+03:00"
---

Problem (aspen, Tender secrets lane): Tender runs secret-tool through lib/process-command.f with the secret on stdin (PROC-CMD:IN!) and reads the value from the captured stdout (OUT$); afterwards the server must erase the library's staging - the stdin copy in PROC-CMD-IN and the captures in PROC-CMD-OUT / PROC-CMD-ERR (lib/process-command.f:46-48) - and today its only door is a second IN! of zero bytes plus overwriting the capture in place, i.e. reaching into the library's buffers. Acceptance: a public PROC-CMD:WIPE ( -- ) that zero-fills the three staging buffers to their full capacities (PROC-CMD-IN-CAP, PROC-CMD-OUT-CAP, PROC-CMD-ERR-CAP) and resets their lengths, callable after any run outcome including a refused spawn; RESET keeps its meaning (lengths only) and no run wipes implicitly - the caller who staged a secret asks. Tests in lib/process-command-test.f: stage bytes, run a child that echoes them, WIPE, then every byte of the three buffers is zero and OUT$/ERR$ are empty; WIPE before any run is a no-op that succeeds; a later run works after WIPE. docs/stdlib.md gains the word next to IN!/OUT$. Files: lib/process-command.f, lib/process-command-test.f, docs/stdlib.md. Verify: bin/hb --load lib/process-command-test.f; the stdlib-process-fixtures row. Depends: none. Ownership: lib (alder). Claim: alder, .jj-ws/alder-process-wipe from eb20cfaa.

Implemented with the existing SPAN:FILL operation over each full capacity,
then three length resets. Argument/environment/CWD/inheritance/outcome state
is retained. Standalone at both tiers, the full six-file stdlib-process-fixtures
row and the four-file tool-boundary-doc-public row pass in a private tree.
The fixture scans every byte after a real stdin echo and taints the unused
tails; deleting only the three fills (leaving length resets) fails nine
assertions. It also pins pre-run and repeated wiping, unchanged RESET behavior,
retained command metadata/outcome, a subsequent run and a refused-run cleanup.
Independent Astra review has no actionable findings. Hazel owns integration.

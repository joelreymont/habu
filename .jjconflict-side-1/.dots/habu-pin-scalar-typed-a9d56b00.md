---
title: Pin scalar typed-local annotation fidelity
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:20:33.527040+02:00"
---

Found by the typed-locals acceptance audit: a checker mutation erasing non-linear local annotations (every {: x:bool :} read as n) would plausibly survive every registered suite - the linear own family pins only linear types. Add a one-line rejects-fixture of the measured shape ( bool -- n ) {: x:bool :} x 1 + (refused expected n n actual bool n, rc 70) to a registered checker suite. Files: test (checker suite). Depends: none.

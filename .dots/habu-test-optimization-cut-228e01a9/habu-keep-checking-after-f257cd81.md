---
title: Keep checking after a call to a malformed qualified name
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T12:52:04.451320+02:00"
---

Under tools/check.f --all-errors (prose and --json-errors), ': GDX-MAL-CALL ( -- ) GDX:MAL:CALL ;' followed by ': GDX-BAD ( -- ) 1 ;' reports only the E-BAD-QUALIFIED definition record for gdx-mal-call, rc 70; GDX-BAD's E-MISMATCH is never reported. Reversed order reports both. Measured identical on batch 4 (a2a0b3d3) and batch 4b (review 432), so it predates the malformed-name lane. A refused call inside a definition is an ordinary definition refusal; --all-errors must report every later definition, as it does after other definition refusals. Acceptance: both definitions reported in both modes, rc 70, through tools/check-test-lib.f, seen failing first.

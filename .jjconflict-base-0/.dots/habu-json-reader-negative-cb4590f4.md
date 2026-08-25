---
title: JSON reader negative-fixture batch
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T14:32:22.664413+02:00"
---

Problem: behaviors proven fail-closed live but unpinned by any suite (audit + pair-review 2026-07-22): empty input -> E-JR-EOF; truncated containers '[1,' and '{"a":' -> E-JR-EOF; raw control byte in string -> E-JR-STRING; leading-zero int '01', lone '-', '1.', '1e' -> E-JR-NUMBER; oversized-key MISS path (skip unrelated >256-byte key then FIND a later key -> FOUND, value correct) — the exact demonstrated pre-03fe0bdc defect, currently only hit-path lengths are pinned; E-JR-EOF reached mid-FIND-KEY on truncated '{"a":1,'. Optional: JR:INIT on storage already backing a live reader silently re-initializes, aliasing the old handle — add an active-reader guard or document + test the contract. Acceptance: each case a T{ }T/TTHROWSQ fixture in lib/json-read-test.f asserting the exact named code; oversized-key miss-path asserts FOUND + value. Files: lib/json-read-test.f (+ lib/json-read.f only if the INIT guard is taken). Verify: bin/hb --load lib/json-read-test.f. Depends: codex pair 03fe0bdc/ca1d4c65 merge (fixtures target the post-pair reader). Ownership: json-read negative fixtures. Claim: unassigned.

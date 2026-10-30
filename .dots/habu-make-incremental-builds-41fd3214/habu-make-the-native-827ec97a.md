---
title: Make the native-file tests assert the fasl behaviour
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T10:48:15.333609+02:00"
---

Problem: 31 test files name replay-era diagnostics or words (segment-stale, object-stale, native-file: reused/hit, REPLAY-*, reused=, files reused): heaviest test/native-file-state-e2e.f 27 hits, native-file-replay-e2e.f 25, native-file-types-e2e.f 23, native-file-auto-e2e.f 18, runtime-regression-test.f 12, native-file-retired-e2e.f 9, native-file-slots-e2e.f 8, compiler/native-prefix-declarations.f 8; lane 8 saw test/native-file-frame-e2e.f exit 67 on the spike. Under the fasl model segments, slots and owner state are not installed, so these tests assert behaviour that no longer exists. Acceptance (lead triage first): each test is kept when it states a requirement that still holds (an object is stored, a refusal is named, warm == cold, an edit compiles only the edited file's heads, a malformed object is refused at open) and rewritten to the spike's lines (native-file: object/miss/live-head/stale-head/head-stale, files reused N compiled M); deleted when it mirrors the replay implementation (segment-stale, slot rows, owner import); the full suite passes on the spike engine. Files: the 31 files from rg -l over test/ with the pattern above. Verify: bin/hb --load test/run.f on the spike engine. Depends: the two preceding dots. Ownership: carl-fasl.

Claim (carl, 2026-10-30): red-row triage of the 11B suite (21 red of 508) in .jj-ws.noindex/carl-l11b, report carl-file/red-triage.md; test lanes follow.

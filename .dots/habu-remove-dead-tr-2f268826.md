---
title: Remove dead TR-SPAWN-CAPTURE block in run-lib
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T18:46:37.693611+02:00\""
---

Surfaced while landing habu-gate-must-test-cd70ef4e: test/run-lib.f keeps a dead dispatch family - TR-SPAWN-CAPTURE (two literal bin/hb spawn sites near :761), its only caller TR-RUN, and the seven resident wrappers (TR-STDLIB, TR-ENGINE, TR-DICTIONARY, TR-DIAGNOSTICS, TR-DEBUG, TR-AOT-POSITIVE, TR-AOT-NEGATIVE) - with zero repo-wide callers; live dispatch is the phase pool (GT-POOL-START). The dead block hides literal bin/hb spawn sites that read like gate-soundness holes during audits while driving nothing. Remove the whole family, re-run the full gate (test/run.f) to prove nothing referenced it, and confirm the literal bin/hb census in the file shrinks by exactly the removed sites. Territory: test/run-lib.f only; serialize behind any open lane editing that file.

Claim: agent=dead-tr workspace=.jj-ws/habu-remove-dead-tr-2f268826

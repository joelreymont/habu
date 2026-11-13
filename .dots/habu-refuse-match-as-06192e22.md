---
title: Refuse ;match as a definition name
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:27:50.743102+02:00"
---

Problem: src/habu/interpreter.f DEF-WALL (1539) admits ': ;match ( -- ) ;', but every body call to it fails 'misplaced match token': body compilation always takes ;match for itself (hunt 633076da; on the installed hb (2026-11-13) `: ;match ( -- ) ;` defines and `: U ( -- ) ;match ;` fails 'bad match: misplaced match token', rc 70). Acceptance: defining ;match is refused by the definer as other control words are (DEF-KEYWORDS 1453), with its arm64 twin habu2.f EMIT-DEF-KW-GUARD if that still runs (habu2.f no longer defines it). Files: src/habu/interpreter.f, src/habu/habu2.f. Verify: one-line file under bin/hb --load refused; test beside the existing DEF-KEYWORDS refusal cases. Depends: none. Ownership: interpreter.f DEF-KEYWORDS. Claim: unassigned.

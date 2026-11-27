---
title: Reject a member list that names a member twice
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:01:15.433061+01:00"
---

Problem: src/lib/json/read.f READ-LISTED and src/lib/json/value.f EACH-LISTED read the same blank-separated list but differ when it names a member twice ("a a" over {"a":1}): READ-LISTED rejects the object at its `}` (KEY-INDEX marks only the first index, REQUIRE-ALL finds the second unseen), EACH-LISTED accepts it (HAS? finds "a"). A list repeating a name is the program's mistake, not the text's. Acceptance: such a list is rejected where the program writes it, statically, with no per-read check, and both words then read every list alike. Depends: none. Claim: unassigned.

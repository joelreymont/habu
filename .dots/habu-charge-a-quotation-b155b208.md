---
title: "Charge a quotation literal's facts to its runner"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T17:05:52.673551+01:00"
---

Problem: a quotation literal's may-flags are charged to the word that writes it, not to the word that runs it: ': EVC ( -- ) s" : NEW ( -- ) ;" evaluate-closed ;' / ': Y ( -- ) [: EVC ;] drop ;' / 'Y' / 'NEW' is rejected E-UNDEFINED at NEW by --load, while --verify-only takes Y as a renderer, leaves the rest to the run and exits rc 0. Acceptance: a word that only writes or drops a quotation takes none of its facts; the three readers agree on the program above. Depends: habu-carry-a-may-c9f6fc75. Claim: unassigned.

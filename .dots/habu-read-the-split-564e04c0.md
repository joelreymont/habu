---
title: Read the split-str marker as the keyword is read
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:06:11.569991+01:00"
---

Problem: the split-str keyword matches ignoring case, but src/habu/verify-source.f SPLIT-STR-FILE? (and the interpreter's marker test) read the first line byte for byte, so a file whose line 1 is SPLIT-STR, or ends in CR, is rejected at its own marker. Acceptance: the marker test and the keyword agree, one rule, pinned by split-str-test. Depends: none. Claim: unassigned.

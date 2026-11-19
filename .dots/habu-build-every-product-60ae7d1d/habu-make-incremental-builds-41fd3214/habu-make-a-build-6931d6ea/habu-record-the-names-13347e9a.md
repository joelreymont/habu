---
title: Record the names a compile looks up
status: closed
priority: 2
issue-type: task
created-at: "2026-11-19T00:01:14.587364+01:00"
closed-at: "2026-11-19T14:58:30.127138+01:00"
---

Problem: a lookup of a computed name (CHECKER-RESOLVE:WL-PROBE, xref.f's by-name scans) is no dependency of the part that asked. Acceptance: compile-reads.f records each such lookup's name and wordlist as a NAME-READ entry, the linker adds them to the part's NAMES (a private wordlist's name opens its package for the match), a lookup under WL:RETIRED makes the part never link; a test in test/file-objects.f builds the dot's reproducer through WL-PROBE cold and warm and both print the same; the cold engine build and a one-file warm rebuild are measured before and after. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-nr.

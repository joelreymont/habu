---
title: Bake the position a cold build would, warm
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:56:43.985608+01:00"
---

Problem: a top-level 'get-current constant W', 'ndict@ constant N' or 'here constant H' in part B bakes a wordlist number, a record count or a data address; a design read (dot habu-make-a-build-6931d6ea) inferred that when an earlier part A adds a wordlist, a record or data, a cold build gives B a new value and a warm build links B's object with the old one. Not reproduced. Acceptance: a test in test/file-objects.f builds such a B after A grows, cold and warm, and both print the same values. Depends: none. Claim: unassigned.

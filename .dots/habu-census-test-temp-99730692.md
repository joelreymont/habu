---
title: Census test temp roots that no cleanup removes
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T06:37:18.075684+02:00"
---

262 FS:MKTEMP-DIR sites under test/ and tools/ against 26 FS:REMOVE-TREE and about 20 FS:CLEANUP-RUN users; test/native-file-e2e.f leaked 9 GB in a day before registering its root (habu-remove-the-native). List each helper or row whose temp root reaches process exit unregistered, and fix each at its helper with FS:CLEANUP-TREE+. Claim: unassigned.

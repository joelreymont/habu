---
title: "Sync a created file's directory only when it changed"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T13:49:55.866775+01:00"
---

Problem: src/lib/fs-mutate.f CREATE-SCOPE syncs the directory even when no name was published (an ext4 journal commit, 2.5-6 ms per Maki apply step whose files all exist), and ATOMIC-SYNC-DIR first fsyncs Linux's O_PATH descriptor, which always fails EBADF. Acceptance: ATOMIC-CREATE-FILES syncs the directory once when it published a name and not otherwise, with no failed fsync on Linux; the fs-mutate suite passes. Depends: none.

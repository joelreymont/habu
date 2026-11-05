---
title: "FS:ATOMIC-CREATE-PRIVATE: write a private file only when its name is free"
status: open
priority: 1
issue-type: task
created-at: "2026-11-05T12:51:19.636771+02:00"
---

From stuart (Tender), 2026-11-05. Joel approved the word in Tender DECISIONS.md ("built on a public Habu write-once word") and chose its name 2026-11-05: FS:ATOMIC-CREATE-PRIVATE ( ptr u8 n ptr u8 n -- bool ). Writes the text owner-only (0600) through a synced unique sibling, claims the path with link(2) (private RENAME-NOREPLACE, src/lib/fs-mutate.f:177), syncs the directory as ATOMIC-WRITE-FILE does after habu-fs-atomic-write-32d1017b; answers false and leaves the path untouched when it exists. Caller: Tender src/admission-store.f PUT (:165), now FS:ATOMIC-WRITE-PRIVATE (:101). Files: src/lib/fs-mutate.f, src/lib/fs-mutate-test.f. Depends: habu-fs-atomic-write-32d1017b. Ownership: carl. Tell stuart when it lands.

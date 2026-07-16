---
title: "FS: make atomic replace alias-safe"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-16T19:59:03.109756+02:00\""
---

Full context: lib/fs-mutate.f ATOMIC-WRITE-FILE always opens the fixed destination-plus-.tmp path through WRITE-ALL with create+truncate, so a pre-existing symlink or hardlink can redirect/truncate an authoritative file before rename. This defeats framed change-file transactional publication. Replace it with one checked same-directory atomic-replace primitive that creates a fresh temp inode using target-owned O_CREAT|O_EXCL flags and bounded collision retry, never follows or truncates an existing path, writes all bytes, preserves write/close/cleanup failures structurally, fsyncs the file and owning directory where the target supports it, then renames over the destination only after success. Failure leaves the prior destination byte-identical and removes only the temp inode this call created. Reject NUL/invalid paths before syscalls. Add real symlink, broken-symlink, hardlink, collision, partial-write, close, fsync, rename, cleanup, combined-error, existing-destination, and concurrent-writer fixtures plus macOS/Linux constant parity. Update lib/fs-mutate docs/tests/manifest/FILEMAP. This must land before habu-tools-frame-diff-e98f8a6a closes; no tool-local publish workaround.

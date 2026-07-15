---
title: "FS: checked no-follow reader"
status: active
priority: 1
issue-type: task
created-at: "2026-07-15T12:38:52.842032+02:00"
---

Full context: checked repository scanners must stream a regular file without ever following a final-component symlink. lib/fs.f exposes READ-ALL, which follows symlinks, and no platform-owned O_NOFOLLOW constant or checked open/read abstraction; tool-local macOS/Linux flag literals duplicate ABI policy and permit drift. Add the target-owned no-follow open flag in the OS/FS boundary, expose a strictly typed checked streaming/read primitive that opens with O_NOFOLLOW, verifies the opened descriptor is regular, propagates open/read/close failures without masking, and has no fixed file-size ceiling. Add macOS/Linux mirror checks plus real fixtures for regular, empty, symlink to file, broken symlink, directory, missing path, injected read/close failure, and swap-at-open protection. Update stdlib manifest/docs/FILEMAP. This blocks habu-tools-bulk-diff-f36d0508. No scanner edits or host-language code. Claim: agent=fs-nofollow workspace=.jj-ws/habu-fs-checked-no-7b20610f.

---
title: "FS: checked no-follow reader"
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T12:38:52.842032+02:00"
blocks:
  - habu-epic-one-structure-04f9804f
---

Full context: checked repository scanners must stream a regular file without ever following a final-component symlink. lib/fs.f exposes READ-ALL, which follows symlinks, and no platform-owned O_NOFOLLOW constant or checked open/read abstraction; tool-local macOS/Linux flag literals duplicate ABI policy and permit drift. Add the target-owned no-follow open flag in the OS/FS boundary, expose a strictly typed checked streaming/read primitive that opens with O_NOFOLLOW, verifies the opened descriptor is regular, preserves primary and close failures together without masking, and has no fixed file-size ceiling. Primitive effects must retain nominal fd and rc roles at the checker boundary. The public payload ENUM outcome must use the hard-cutover unified ENUM compiler through native, recovery, AOT, and fixpoint lowering; never rewrite it as SUMTYPE. Generated public constructor words must be documented and exercised through real consumers, or constructor visibility must be explicitly owner-only with external MATCH proven. Add macOS/Linux mirror checks plus real fixtures for regular, empty, symlink to file, broken symlink, directory, missing path, injected stat/read/callback/close and combined primary-plus-close failure, and swap-at-open protection. Update docs and FILEMAP. This blocks habu-tools-bulk-diff-f36d0508. No scanner edits or host-language code. Claim: agent=fs-nofollow workspace=.jj-ws/habu-fs-checked-no-7b20610f.

Milestone routing: M2 only; blocked until M1 is closed on green master.
Claim released for milestone sequencing; preserve the existing workspace until M2.

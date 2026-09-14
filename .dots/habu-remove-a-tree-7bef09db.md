---
title: Remove a tree that holds a socket or a fifo
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T17:28:13.101911+03:00"
---

Problem: lib/fs-mutate.f FS-MUT-REMOVE-TREE-PATH removes symlinks, regular files and directories and throws E-FS-STAT (-2101) for any other inode, so a directory holding an AF_UNIX socket or a fifo cannot be removed by REMOVE-TREE at all; a caller must know every socket a child process may leave (Maki's KiCad harness must unlink the api socket and the X socket by name before REMOVE-TREE, and only the kernel knows what else a program leaves in a TMPDIR). Acceptance: REMOVE-TREE unlinks every non-directory inode it meets (socket, fifo, device node it may unlink) and descends only into directories; a test creates a directory holding a bound unix socket and a fifo and asserts REMOVE-TREE removes the tree; the refusal stays named for what unlink itself refuses. Files: lib/fs-mutate.f, its test. Verify: the Habu suite. Depends: none. Ownership: lib/fs-mutate.f (Habu). Claim: unassigned. Asked by Maki (rowan, 2026-09-14): harness test/kiapi-server.f SHUT-DIR cites this ask beside its socket-only unlink.

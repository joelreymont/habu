---
title: Remove a tree that holds a socket or a fifo
status: closed
priority: 2
issue-type: task
created-at: "2026-09-14T17:28:13.101911+03:00"
---

Problem: lib/fs-mutate.f FS-MUT-REMOVE-TREE-PATH removes symlinks, regular files and directories and throws E-FS-STAT (-2101) for any other inode, so a directory holding an AF_UNIX socket or a fifo cannot be removed by REMOVE-TREE at all; a caller must know every socket a child process may leave (Maki's KiCad harness must unlink the api socket and the X socket by name before REMOVE-TREE, and only the kernel knows what else a program leaves in a TMPDIR). Acceptance: REMOVE-TREE unlinks every non-directory inode it meets (socket, fifo, device node it may unlink) and descends only into directories; a test creates a directory holding a bound unix socket and a fifo and asserts REMOVE-TREE removes the tree; the refusal stays named for what unlink itself refuses. Files: lib/fs-mutate.f, its test. Verify: the Habu suite. Depends: none. Ownership: lib/fs-mutate.f (Habu). Claim: unassigned. Asked by Maki (rowan, 2026-09-14): harness test/kiapi-server.f SHUT-DIR cites this ask beside its socket-only unlink.

Resolved: REMOVE-TREE classifies the non-following stat mode as directory or
non-directory, so sockets, FIFOs and other inode kinds reach the existing unlink
operation. Missing-root and symlink handling, E-FS-STAT for a failed stat,
E-FS-IO for failed unlink/rmdir, and directory-descriptor cleanup remain intact.

Validation: lib/fs-mutate-test.f creates a real bound Unix socket with socat and
a nested FIFO with mkfifo through checked argv/process helpers. The socket child
is terminated and reaped after removal or refusal; readiness has a one-second
bound. Both executable names are resolved through PATH with a named refusal if
missing. The unchanged outside-directory symlink test preserves its target.
The new fixture on the original implementation fails with -2101 and the tree
remaining; its failure cleanup reaps the child and removes the owned tree.
The complete focused fixture passes at ordinary and tier 1 loading on
/tmp/cedar-native-stack-bounded, SHA-256
ca8ee6687f4d6c1f796cc15c7db6ed3ef16269080ab9a5bdee82e9dbb20f9125.
Parent integration owns the full suite; no compiler change or new trust boundary.

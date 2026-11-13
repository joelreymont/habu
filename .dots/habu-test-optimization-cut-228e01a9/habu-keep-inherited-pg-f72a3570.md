---
title: Keep inherited PG connections out of a child
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T18:10:14.132865+03:00"
closed-at: "2026-11-04T11:48:50.274113+02:00"
close-reason: "Census 2026-11-04: goes with habu-fork-only-to-59d3557e, fork only to exec (docs/design.md \"Machinery being replaced\"). Inherited libpq connection finished by a forked child's exit path; an exec'd child runs no PQfinish (docs/design.md \"Machinery being replaced\"); CLOEXEC is the residue [i]"
---

Found by lane 449 forklocks: a forked child inherits the parent's libpq connections; if the child's exit path runs PQfinish on them, the server ends the parent's session (shared socket). Fix: the child must never finish or use an inherited connection (mark connections owner-pid and skip/close-without-terminate in a child, via the FORK-CHILD registry). Acceptance: E2E with a pg row: parent opens a connection, forks a child that exits through its normal path, parent's next query succeeds (fails before).

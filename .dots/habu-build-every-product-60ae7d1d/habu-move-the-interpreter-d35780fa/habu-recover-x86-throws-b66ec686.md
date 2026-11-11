---
title: Roll back a Habu-loop throw without lowering while a task is live
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.397075+03:00"
---

Problem: when an evaluate or include that defined something throws, the Habu loop's rollback (src/habu/interpret.f FRAME-ROLLBACK-CODE :318-325, FRAME-ROLLBACK-TOKEN :338-343) lowers the dictionary count and the code pointer through `ndict!` and CODE-RECLAIM:TRUNCATE (src/habu/xref.f:778, which ends in `cp!`). Both sinks refuse while any task is live: arm64 habu1.f BCPSET :1683 and BNDSET :1710 run B-LOWER-GUARD (exit $4F), x86-64 kernel-x64.f:3925 and :4024 run TASK-LIVE-GUARD, (exit 79). So, read from the tree and not yet run, such a throw with a task live ends the process with 79 instead of reaching the catch. arm64's engine rollback instead lowers nothing while a task is live and retires the records above the mark in place (EM-DEF-OCC:LRETIRE at habu2.f:4463, :12533, :12663), by the rule of efc74616: "while a task is live, a failed evaluate or REPL line and a checker replay's close lower nothing, since a task may run the code and read the records above the mark". Parked commit d00241479c ("Restore evaluate cursors with live tasks", on the unmerged bookmark `intel`, not on master) took the other road: a `frame-rewind` primitive that lowers through unguarded twins of the sinks. That contradicts the rule above, so it is not to be rebased as is. Reproducer, unrun: load the task prelude of habu-make-outer-interpret-267c9a3d (spin.f), then test/interpreter-loop-on.f, then a file with `OI-SPIN s" : OI-X ( -- n ) 1 ; 42 throw" OI-PARK ' OI-EVAL-PARKED catch . 7 .`, where spin.f also defines `TRUSTED: OI-EVAL-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ evaluate ;`; expected stdout `42\n7\n`, exit 0. This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: with a task live, the Habu loop's rollback lowers neither count nor code pointer and retires the records above the mark, as arm64's engine does; the reproducer prints `42` and `7` and exits 0 on arm64. x86-64 gets the same result once habu-lift-the-task-754c94f6 lets it compile while a task is live.

Files: src/habu/interpret.f, and the retire entry the Habu loop needs beside habu2.f EM-DEF-OCC:LRETIRE.

Verify: the reproducer under bin/hb on arm64; if it exits 0 before any change, report that and close this dot with that evidence.

Depends: none.

Ownership: unassigned. Claim: unassigned.

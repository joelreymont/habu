---
title: Roll back a Habu-loop throw without lowering while a task is live
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.397075+03:00"
---

Problem: when an evaluate or include that defined something throws, the Habu loop's rollback (src/habu/interpreter.f FRAME-ROLLBACK-CODE :2473-2489, FRAME-ROLLBACK-TOKEN :2492-2498) lowers the dictionary count and the code pointer through `ndict!` and CODE-RECLAIM:TRUNCATE (src/habu/xref.f:779, which ends in `cp!`). Both sinks refuse while any task is live: arm64 habu1.f BCPSET :1569 and BNDSET :1595 run B-LOWER-GUARD (:1544, exit $4F), x86-64 `cp!` (kernel-x64.f:3969-3970) and NDICT-SET-BODY (:3892-3894) run TASK-LIVE-GUARD, (exit 79). So, read from the tree and not yet run, such a throw with a task live ends the process instead of reaching the catch. arm64's engine rollback instead lowers nothing while a task is live and retires the records above the mark in place (EM-DEF-OCC:LRETIRE, habu2.f:1455, :2920), by the rule: "while a task is live, a failed evaluate or REPL line and a checker replay's close lower nothing, since a task may run the code and read the records above the mark". Commit d00241479c ("Restore evaluate cursors with live tasks", on the bookmark `intel`, not on master) took the other road: a `frame-rewind` primitive that lowers through unguarded twins of the sinks. That contradicts the rule above, so it is not to be rebased as is. Reproducer, unrun: load the task prelude of habu-make-outer-interpret-267c9a3d (spin.f), then test/interpreter-loop-on.f, then a file with `OI-SPIN s" : OI-X ( -- n ) 1 ; 42 throw" OI-PARK ' OI-EVAL-PARKED catch . 7 .`, where spin.f also defines `TRUSTED: OI-EVAL-PARKED ( -- ) OI-PARKED @ OI-PARKED-U @ evaluate ;`; expected stdout `42\n7\n`, exit 0. This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: with a task live, the Habu loop's rollback lowers neither count nor code pointer and retires the records above the mark, as arm64's engine does; the reproducer under bin/hb prints `42` and `7` and exits 0 on arm64 (if it does before any change, close this dot with that evidence). x86-64 gets the same result once habu-lift-the-task-754c94f6 lets it compile while a task is live.

Depends: none.

Claim: unassigned.

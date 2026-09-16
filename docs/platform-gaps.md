# Platform gaps seen from SwiftForth, polyFORTH and VFX Forth

The plan is for Habu to be the base on which generated code runs everywhere:
servers and backends, embedded targets and microcontrollers, with one
language and one vocabulary. Reviewing three mature Forth products on
2026-09-16 (see [tasking-models.md](tasking-models.md),
[socket-models.md](socket-models.md) and
[database-models.md](database-models.md)) gave a concrete list of what they
have and Habu does not yet. This page is the inventory, in priority order,
with the model to follow and the dot that tracks it. Facts about the products
are restated in our words; the manuals are not copied.

## What Habu already does better

The checker, nominal types, algebraic data types, packages, typed foreign
calls, image capture and the deterministic gate have no counterpart in the
three products. Every gap below is a library or kernel service, not a
language gap. That is the right place to be: the language is the part
generated code depends on most.

## Gaps

| Priority | Gap | Follow | Dot |
| --- | --- | --- | --- |
| 1 | A worker throw ends the process | SwiftForth trampoline | `habu-contain-a-worker-fe0c8eb8` |
| 1 | No blocking wait, only polling | SwiftForth/VFX semaphore | `habu-add-a-blocking-fd79b713` |
| 1 | No TCP | SwiftForth verbs, `UDP4` typing | `habu-add-tcp-sockets-fb1d351e` |
| 2 | No result from a joined task | VFX exit code and cleanup, Habu `result` | `habu-return-a-typed-b1c342cd` |
| 2 | No channel between tasks | VFX mailbox and ring queue | `habu-add-task-msgs-c5a6af71` |
| 2 | Text I/O bound to the terminal | VFX generic I/O devices | `habu-route-text-i-1fbcb2ba` |
| 2 | No records, indexes or SQL | polyFORTH kit, VFX SQLite binding | `habu-decide-the-db-4bb703ee` |
| 3 | No tasks without an OS | SwiftX Cortex-M tasker: polyFORTH `STATUS`/`WAKE` ring in 142 lines | `habu-decide-the-cooperative-b463cc1c` |
| 3 | No x86_64 code generation | own backend | planned separately |
| 3 | HTTP and TLS | `libcurl` through the FFI, a proxy for servers | after TCP |
| 4 | Events (software interrupts into a task) | VFX `SET-EVENT` | after the queue |
| 4 | Binding generation from C headers | VFX's mechanical `sqlite3h.fth` shows the need | with the database decision |
| 4 | Reference manual generated from source | VFX DocGen comments; Habu has typed signatures to draw from | later |

## Two structural ideas worth taking whole

**One task vocabulary on both kernels.** VFX keeps the MPE embedded task API
on pthreads so the same program text runs hosted and on a target. Habu should
do the same: the hosted `TASK` package and the cooperative kernel for targets
expose the same public words; only the implementation and the hosted-only
extras differ. The cooperative kernel is polyFORTH's: about thirteen words,
the `STATUS` cell as both scheduler link and wake flag, and every blocking
I/O word yielding.

**Generic I/O devices.** VFX routes `EMIT`, `KEY`, `TYPE` and `ACCEPT`
through a per-task device record, so a socket or a serial port becomes the
REPL's console without touching the REPL. That is how the running system on a
server or a microcontroller stays interactive and debuggable from a distance,
which is the property the whole plan rests on: generated code lands in a
live system that can be inspected and corrected in place.

## Where the source material is

Local copies as of 2026-09-16, outside the repository because of their
licences: `~/Downloads/forth-inc/` holds the SwiftForth 4.1.10 Linux
evaluation (manual and kernel source), the polyFORTH DB005 manual, the SwiftX
application note, under `swiftx/` the SwiftX ARM evaluation (reference manual,
ARM target manual, target and library source; its Inno Setup 6.6.1 installer
opens only with innoextract pull request 214, kept as `tools/innoextract-pr214`),
and under `mpe/` the VFX Forth 64 Linux community edition with its manual and
library source. Section and page references are in the
three model documents.

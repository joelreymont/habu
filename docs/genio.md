# Generic text I/O devices

[`lib/genio.f`](../lib/genio.f) puts a vector table between Habu's text words
and the process terminal, so `EMIT`, `TYPE`, `KEY`, `ACCEPT` and the REPL that
is built from them work over a socket, a serial link or a buffer without
changing a line of the code that calls them. The model is VFX Forth's generic
I/O layer, restated in [socket-models.md](socket-models.md) section 2.

## The model

A **device** is eight operations plus one cell of its own data:

| Operation | Effect | Meaning |
| --- | --- | --- |
| emit | `( c -- )` | send one character |
| key | `( -- c )` | receive one character |
| key? | `( -- bool )` | is a character waiting, without waiting |
| read | `( ptr u8 n -- n )` | one transfer into the span; `0` is the end of input |
| write | `( ptr u8 n -- )` | send the whole span |
| accept | `( ptr u8 n -- n )` | one line without its terminator; `0` is the end of input |
| flush | `( -- )` | deliver anything held back |
| close | `( -- )` | release the device's own resource |

`GENIO:DEVICE` takes those eight quotations in that order followed by the state
cell and answers a `GENIO:device`. The handle is a **row number and a
generation**, never an address: 0 is the built-in terminal and
`GENIO-ABI:DEVICES` (8) further rows are available. A process has a terminal,
perhaps a connection or two and a capture buffer, and a fixed table costs no
allocation — which is also what lets this layer run on a microcontroller.

`GENIO:CLOSE` gives the row back, so the eight rows are a pool rather than a
budget: open and close as many devices as the program needs. Closing bumps the
row's generation, so a handle minted before the close is **refused with
`E-GENIO-STATE`** by `GENIO:CLOSE`, `GENIO:OUTPUT!`, `GENIO:INPUT!` and
`GENIO:WITH-IO` instead of being routed to whatever device took the row next.
Re-registering a reused row re-declares the same engine cell, which the mark
path answers from its index, so reclaiming a row does not move the cost into
the declared-address table.

The eight operations are shared code — two TCP devices are the same eight
quotations over two connections — so an operation asks `GENIO:SELF` which row it
is running for and `GENIO:SELF-STATE` for that row's cell.

## Public words

```forth
GENIO:DEVICE   ( q q q q q q q q n -- device )  \ emit key key? read write accept flush close state
GENIO:TERMINAL ( -- device )
GENIO:TCP-DEVICE ( TCP4:connection -- device )

GENIO:OUTPUT!  ( device -- )      GENIO:OUTPUT@ ( -- device )
GENIO:INPUT!   ( device -- )      GENIO:INPUT@  ( -- device )
GENIO:WITH-IO  ( input output [ -- ] -- )

GENIO:EMIT     ( c -- )           GENIO:KEY    ( -- c )
GENIO:TYPE     ( ptr u8 n -- )    GENIO:KEY?   ( -- bool )
GENIO:WRITE    ( ptr u8 n -- )    GENIO:READ   ( ptr u8 n -- n )
GENIO:FLUSH    ( -- )             GENIO:ACCEPT ( ptr u8 n -- n )
GENIO:CLOSE    ( device -- )
GENIO:READ-LINE ( -- ptr u8 n )
GENIO:SELF     ( -- device )      GENIO:SELF-STATE ( -- n )
```

`GENIO:WITH-IO` switches both devices, runs the quotation and restores the
caller's devices whether it returns or throws; the throw is re-raised after the
restore. If the quotation closed the device the caller was on, the restore puts
that task on the terminal rather than failing over the body's own result.

**Closing a device that is current leaves that task on the terminal.** It is
not an error, and it cannot be one: the engine's output funnel reaches a device
write from inside `emit`, where a throw would unwind through engine internals,
so it falls back to the terminal when the row it names is empty. `GENIO:EMIT`
and `GENIO:TYPE` agree with it, because a `type` and a `GENIO:TYPE` that
disagreed about where a closed device's output went would be worse than either
answer. `GENIO:OUTPUT@` and `GENIO:INPUT@` answer the terminal once the row
they named has been closed.

## Per task, and inherited

The current input and output devices are two cells of the engine's `DATA`
header ([`src/habu/layout.f`](../src/habu/layout.f) `GENIO-ABI`). The engine
swaps `DATA` for each task, so those cells are per task with no lookup, and
[`lib/task.f`](../lib/task.f) `TASK-REGION-INIT` copies them and the write table
into a new task's region: **a task starts on its creator's devices**. Copying is
sound precisely because the cells hold an index — a freshly mapped task region
has nothing that relocates a code address.

## What the engine routes, and what it does not

`emit`, `type`, `.`, `u.`, `cr`, `space`, interpreted `."` and `.\"`, and the
REPL's `ok` are engine primitives that used to write to descriptor 1 directly.
They now funnel through one routine (`G-OUT` in
[`src/habu/rt.f`](../src/habu/rt.f), with the device half registered as the
sealed engine helper `(GENIO-OUT)`), which dispatches to the current output
device. The terminal path costs one load and one compare; the device path is a
call. That is what makes a remote REPL free: the answer to a query is printed by
`.`, and `.` follows the device.

**Diagnostics stay on descriptor 2.** `src/habu/crash.f`, `die`, the `E-*`
reports and the REPL's error line write through the syscall and are never
routed, so a failing engine still reports on the descriptor it was started with
however a program has routed its output.

`src/habu/repl.f`'s line editor echoes through `emit` and `type`, so a line
edited over a connection is echoed back over it. `KEY1` and `RD-LINE` stay on
descriptor 0 because together they *are* the terminal device's key and accept.
Requiring `lib/genio.f` installs `GENIO:READ-LINE` as `REPL-READ`, the vector
the engine's own REPL loop reads a line through, so the REPL follows the current
input device with no change to `repl.f`.

## Several tasks, one registry

The device table is shared dictionary storage, so two tasks building devices at
once is ordinary — a task that accepts a connection and builds a device on it is
the normal case. A row is therefore **claimed by compare-and-swap on its own
cell**, never by a read-then-write another task could interleave with: `FREE ->
LIVE` takes a row, and `LIVE -> CLOSING` earns the right to close one, so two
tasks closing the same handle cannot both run the close operation and both move
the generation on — one wins and the other is told the handle is gone.

A device's operations, state and write row are filled by the task that claimed
the row, after the claim and before any handle for it exists. **A device is
built and closed by one task**: its write row lives in that task's `DATA`, which
is what makes the funnel's dispatch per task, and a child inherits the table as
it stood when it was created. Closing from a different task than the builder
clears the wrong task's row, so don't.

## Errors

`E-GENIO-OPERAND` (`-9220`) is a handle whose row is outside the table, or a
span that will not hold the answer; `E-GENIO-STATE` (`-9221`) is a handle to a
device that is gone — a closed row, or a generation that has moved on;
`E-GENIO-IO` (`-9222`) is the device's own failure; `E-GENIO-FULL` (`-9223`) is
`GENIO:DEVICE` with no row free, which is not a broken table but a caller that
has to close a device before opening another.

**A write operation must not throw.** The engine reaches it from inside `emit`
and `type`, where a throw would unwind through engine internals with the
funnel's re-entrancy guard still set. A device that fails a write latches the
failure, and every later operation on that device raises `E-GENIO-IO` — a
device whose write failed is broken, and it stays broken until the row is built
again. `GENIO:CLOSE` still works, so a failed device can still be released.
Every other operation is called from ordinary Habu code and reports at once.

The funnel falls back to descriptor 1 in three cases, none of them silent
corruption: a device write that itself reached `type` (the re-entrancy guard), a
routing cell outside `1..DEVICES`, and a row nothing has registered. Output has
to keep working while a device is being built or has just gone away.

## The TCP connection device

`GENIO:TCP-DEVICE` takes a connected [`TCP4:connection`](tcp4.md). Emit and
write send, key and read receive, `key?` is `TCP4:READABLE?`, flush has nothing
to do because a write is a send, and close is `TCP4:CLOSE`. `accept` reads to
the next newline one byte at a time, because a device keeps no buffer of its own
in which half a line could be stranded for the next reader.

A peer that goes away **fails**; it does not spin. `KEY` and `READ` cannot
answer with a byte once the stream has ended, so both raise `E-GENIO-IO` for an
ended or failed stream. `ACCEPT` answers zero instead, which is the end of input
a line loop stops on — the same answer the terminal gives on `^D`.

## A REPL over a connection, and the one restriction

```forth
: SERVE-LINE ( -- bool )
   REPL-READ {: line:ptr len:n :}
   len 0 = if false exit then
   line len INCLUDE-EVALUATE
   s" ok" type cr
   true ;

: SERVE ( -- ) begin SERVE-LINE while repeat ;

connection GENIO:TCP-DEVICE {: dev:GENIO:device :}
dev dev [: SERVE ;] GENIO:WITH-IO
```

**No Habu task may be live while such a REPL compiles.** A colon definition is
dictionary mutation, and Habu forbids that while any task is live: the engine
exits `$4F` naming the rejected token (see [threads.md](threads.md)). A remote
REPL on a server that is running worker tasks can therefore evaluate but not
define. That is a restriction of Habu's tasking model, not of this layer, and it
is why [`lib/genio-test.f`](../lib/genio-test.f) holds both ends of its
connection in the main task.

## Images

A device is a process resource, not a serialisable handle. `lib/genio.f`
registers a reset with [`IMAGE-LIFECYCLE`](../lib/image-lifecycle.f): quiescent
image preparation puts routing back on the terminal, clears the engine's write
table, frees every row and moves every generation on, so no snapshot or
ahead-of-time capture carries a socket, a descriptor or a device's code
address, and no handle from before the capture addresses a row after it. Zero is the answer
both capture paths already understand — the snapshot rebase leaves a zero
declared cell zero, and an AOT capture records a zero target rather than
refusing one outside its window. Close your connections before capturing;
descriptors are process resources.

## Checks

```sh
bin/hb --load lib/genio-test.f
```

A memory device of eight byte-buffer operations proves that each operation
dispatches, that an operation is handed its own state cell, and — the point of
the exercise — that `emit`, `type` and `.` themselves land in the device's
buffer, which they can only do through the engine funnel. The suite also covers
`WITH-IO` restoring on a normal return and on a throw, a task inheriting its
creator's device and writing through it, a REPL served over a loopback TCP
connection that compiles a definition and answers a query, and a read through a
closed peer failing with `E-GENIO-IO`. Reclamation has its own cases: four
hundred open-and-close rounds over an eight-row table, a ninth simultaneous
device refused with `E-GENIO-FULL`, a handle refused with `E-GENIO-STATE` after
its device is closed, the declared-address table not growing across those
rounds, and a closed current device leaving the task on the terminal. Three
tasks then run two hundred build-and-close rounds each, asserting that no two
live handles are ever equal — which is what one row handed out twice would look
like.

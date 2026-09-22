# Asynchronous I/O on io_uring

`lib/aio.f` (package `AIO`) lets a task wait for a descriptor, a duration or a
cancellation, and read, write, accept or connect, without parking its thread
inside the host call. Linux only.

## Model

A task **submits** an operation and gets a **ticket**. One completion task
drains the ring and, for each operation that finished, stores its result and
`TASK:WAKE`s the task that submitted it. The submitter **awaits** its ticket:
the await is a `TASK:STOP` loop over the record's state, so it costs no CPU and
no thread of its own.

```forth
AIO:LOOP-START                                   \ once, after compilation
: SERVE ( -- )                                   \ inside a task
   FD AIO:READABLE 500 >MS AIO:POLL-ADD          \ a poll with a deadline
   AIO:AWAIT MATCH AIO:outcome
      ready     OF DRAIN-INPUT ENDOF             \ the revents the kernel gave
      timed-out OF RETRY ENDOF
      cancelled OF STAND-DOWN ENDOF
      refused   OF REPORT ENDOF                  \ the errno, positive
   ;MATCH ;
```

Sixty-four waits from eight tasks cost eight threads and the loop's one -
measured in `lib/aio-test.f` by counting `/proc/self/task` during the run.

The loop is started explicitly because a live task forbids compilation
([threads.md](threads.md)): the program decides when it has finished defining
words. The loop task is no exception: it runs `lib/aio.f`'s code, which sits in
the 64K unit a later definition flips non-executable whenever `lib/aio.f` was
the last thing compiled (measured in threads.md), so a program defines
everything first and starts the loop last. A submission with no loop running is
`E-AIO-STATE`.

`lib/net/tcp4.f`, `lib/net/udp4.f`, `lib/serial.f`, `lib/pty.f`,
`lib/process-pty-io.f` and `lib/signal.f` are the loop's library callers: every
`TCP4` readiness question, every `UDP4:RECEIVE` that has to wait, every
`SERIAL:READ` and `SERIAL:WRITE`, every `PTY:READ`, `PROCESS-PTY:AWAIT` and
`AWAIT-BYTES`, and `SIGNAL:WAIT` and `SIGNAL:PENDING?` is one `POLL-ADD` and one
`AWAIT` here ([tcp4.md](tcp4.md), [udp4.md](udp4.md), [serial.md](serial.md),
[signal.md](signal.md), [process-pty.md](process-pty.md)), so a program that
uses any of them starts the loop before its first wait.

## The surface

| Word | Effect | Blocks |
| --- | --- | --- |
| `AIO:LOOP-START` | `( -- )` | never; a second start is `E-AIO-STATE` |
| `AIO:LOOP-STOP` | `( -- )` | until the loop task has ended |
| `AIO:LOOP-RUNNING?` | `( -- bool )` | never |
| `AIO:READABLE` | `( -- n )` | never; `POLLIN` |
| `AIO:WRITABLE` | `( -- n )` | never; `POLLOUT` |
| `AIO:POLL-ADD` | `( fd n ms -- AIO:ticket )` | never; `ms` -1 for no deadline |
| `AIO:TIMEOUT` | `( ms -- AIO:ticket )` | never |
| `AIO:CANCEL` | `( AIO:ticket -- )` | never |
| `AIO:AWAIT` | `( AIO:ticket -- AIO:outcome )` | until that operation has ended |
| `AIO:READ` | `( fd ptr u8 NUM:alloc-byte-len n n -- AIO:xfer )` | never; count then offset |
| `AIO:WRITE` | `( fd ptr u8 NUM:alloc-byte-len n n -- AIO:xfer )` | never; count then offset |
| `AIO:ACCEPT` | `( fd -- AIO:ticket )` | never |
| `AIO:CONNECT` | `( fd ptr u8 n -- AIO:ticket )` | never; the sockaddr and its length |
| `AIO:AWAIT-XFER` | `( AIO:xfer -- ptr u8 NUM:alloc-byte-len AIO:outcome )` | until that transfer has ended |
| `AIO:CANCEL-XFER` | `( AIO:xfer -- )` | never |
| `AIO:GROUP` | `( -- )` | defines a set of at most `$40` tickets |
| `AIO:GROUP+` | `( AIO:ticket AIO:group -- )` | never |
| `AIO:GROUP-` | `( AIO:ticket AIO:group -- )` | never |
| `AIO:GROUP-COUNT` | `( AIO:group -- n )` | never |
| `AIO:GROUP-MAX` | `( -- n )` | never; the tickets one group holds |
| `AIO:MAX-OPS` | `( -- n )` | never; the records: operations in flight, a poll with a deadline holding two |
| `AIO:AWAIT-ANY` | `( AIO:group -- AIO:ticket AIO:outcome )` | until one of them has ended |

`AIO:outcome` is a layout, so no caller reads a result without deciding what to
do about every arm:

| arm | when |
| --- | --- |
| `ready ( n )` | the operation succeeded; for a poll the revents mask, for a `READ` or a `WRITE` the bytes moved, for an `ACCEPT` the new descriptor |
| `timed-out` | a `TIMEOUT` fired (`-ETIME`), or a `POLL-ADD` ended by its own deadline |
| `cancelled` | an `AIO:CANCEL` ended it (`-ECANCELED`) |
| `refused ( n )` | any other `-errno`, as a positive number |

- A ticket belongs to the task that submitted it: an `AIO:AWAIT` from another
  task is `E-AIO-STATE`, and so is a second await of the same ticket - the first
  one released the record.
- `AIO:POLL-ADD` with `ms` at or above zero links an `IORING_OP_LINK_TIMEOUT`
  behind the poll with `IOSQE_IO_LINK`. The kernel posts both completions - the
  poll's `-ECANCELED` and the timeout's `-ETIME` - so the record owes two and
  settles on the last of them, whichever order they arrive in. That is what
  tells a deadline apart from an `AIO:CANCEL`.
- `AIO:CANCEL` does not wait. The ticket still has to be awaited; it answers
  `cancelled` once the kernel has ended the operation.
- `AIO:AWAIT-ANY` answers the first ticket of the group whose operation has
  ended, and that ticket leaves the group. The others stay. An empty group is
  `E-AIO-GROUP`, and so is a `GROUP-` of a ticket the group does not hold.
- `AIO:LOOP-STOP` refuses with `E-AIO-BUSY` while any record is still in flight:
  unmapping a ring the kernel still owns is not something a caller may ask for.
  Await or cancel everything first. After a stop the ring can be started again.

## Completion operations

A poll says a descriptor is ready; a completion operation does the work. A
regular file has no readiness at all, which is what `AIO:READ` and `AIO:WRITE`
are for.

**The allocation belongs to the transfer.** `AIO:READ` and `AIO:WRITE` take a
`lib/memory.f` allocation - the pointer and the extent exactly as
`MEM:ALLOC-BYTES` answered them - and answer an `AIO:xfer`. From that moment
until `AIO:AWAIT-XFER` answers the pair again, nothing in the module produces
it, so no checked code can read, write or release the bytes the kernel is
moving. The type is the rule: `AIO:AWAIT`, `AIO:GROUP+` and `AIO:GROUP-` take an
`AIO:ticket` and refuse an `AIO:xfer` at compile time, and `AIO:AWAIT-XFER`
refuses a ticket. `lib/aio-test.f` pins both refusals as rejected programs.

A raw pointer the caller kept a copy of before submitting is outside this
design: the checker has nothing to say about it. The unique bounded borrow of
dot `habu-add-unique-bounded-527e05ca` is what would close that hole; until it
lands, hand `AIO:READ` an allocation and forget the pointer.

- `count` is the bytes to transfer and `off` the file offset, or `-1` for the
  descriptor's own position, which the transfer uses and advances the way
  `read(2)` and `write(2)` do. A non-seekable descriptor (a pipe, a socket)
  takes offset `0`.
- The `ready` arm carries the bytes the kernel moved. Fewer than `count` is a
  short transfer and not an error; zero from a `READ` is the end of the file.
- A count above the allocation's extent, a count below zero, or an offset below
  `-1`, is `E-AIO-BOUNDS`. That refusal, `E-AIO-STATE` and `E-AIO-FULL` happen
  before any submission entry is written, so the allocation is still the
  caller's; keep the pair in a local or a cell across the submission if the
  program means to recover from one of them. `E-AIO-ENTER` is different: the
  entry is published, and a published entry the refused `io_uring_enter` did
  not take is consumed by the next one from anyone, so the kernel may yet run
  it. The transfer keeps the allocation and the loop releases it when that late
  completion arrives. A program that catches `E-AIO-ENTER` must not touch or
  release those bytes.
- `AIO:CANCEL-XFER` is `AIO:CANCEL` for a transfer, and just as much a request:
  a cancel of a `READ` on a regular file may lose the race and the outcome is
  then `ready`, because such a read can be served before it is ever cancellable.
- An `AIO:xfer` is not a group member in this slice: `AIO:AWAIT-ANY` waits on
  tickets only.

`AIO:ACCEPT` asks for no peer address, so nothing of the caller's has to outlive
it; its `ready` arm carries the new descriptor, which the caller owns and
closes, and which is close-on-exec like `TCP4:ACCEPT`'s. `AIO:CONNECT`'s `ready`
arm carries zero. **The sockaddr bytes `AIO:CONNECT` names are the caller's and
must stay mapped and unchanged until the ticket's outcome is taken.** This
module does not copy them and states no claim about when the kernel does.
`lib/net/tcp4.f` answers the descriptors: `TCP4:LISTENER-FD`,
`TCP4:CONNECTION-FD` and `TCP4:SOCKET`, which is a stream socket connected to
nothing yet.

**A transfer nobody awaits.** When a task ends with one in flight, the per-task
cleanup described below marks the record forgotten and cancels it, and the loop
releases the allocation when the kernel's
completion for that operation arrives - never before, because until then the
kernel may still be writing those bytes. The loop task is the one that makes
that call, which costs nothing: `MEM`'s release is a `munmap`.

## Waiting, and the hint protocol

`AIO:AWAIT` is a `TASK:STOP` loop, which means it obeys the rule
[threads.md](threads.md) states for one: **WAKE is a hint, STOP waits for a
hint, and the caller re-checks its own state after every STOP**. The await
re-reads the record and stops again when the loop woke it for something else,
and it calls `TASK:PAUSE` in the same loop, so a `TASK:HALT` ends a task parked
in an await.

The main thread has no TCB and parks on package `TASK`'s one main record, so a
program with no tasks of its own can await too.

**`AIO`'s cleanup is one registration in the task's chain.** At its first
submission `AIO` registers a cleanup on the calling task, and `TASK:AT-EXIT`
chains the cleanups a task registers, so the program's own cleanups stand beside
it and the chain runs newest first - a task that submits keeps them whether it
registered them before that first submission or after. The cleanup cancels and
forgets that task's operations in flight, which is what keeps the loop from
waking a TCB whose memory the join has released.

## Trust boundaries

Two, both small and both in this module.

1. **The two system calls.** `io_uring_setup` (425) and `io_uring_enter` (426)
   are declared as two `FUNCTION:` rows over libc's `syscall` in
   `PROCESS-SYMBOLS`. The setup declaration names the 120-byte
   `struct io_uring_params` written by the kernel; the enter declaration passes
   only values. A refused call prints its `FFI:ERRNO` on stderr and throws
   `E-AIO-SETUP` or `E-AIO-ENTER`.
2. **One address crossing.** The submission ring, the completion ring and the
   submission entries are three `mmap`s (`MEM-PROT-RW`, `MEM-MAP-SHARED`, the
   ring fd, at `IORING_OFF_SQ_RING`, `IORING_OFF_CQ_RING` and
   `IORING_OFF_SQES`). One private `TRUSTED:` word turns each returned address
   into the `ptr u8` of that mapping, the way `lib/memory.f`'s
   `MEM-MAPPED>PTR` does for its own. Every later read and write goes through
   an offset-and-width bound taken from the lengths `io_uring_params` reported,
   and a violation of that bound is this module's own defect, so it ends the
   process by name instead of throwing.

Everything else - the SQE and CQE fields, the ring indexes, the record table,
the group rows - is ordinary checked Habu over little-endian byte accessors.
The submission tail store and the completion head store follow a `fence`, and
the completion tail read is followed by one.

## Storage and limits

| thing | size |
| --- | --- |
| submission entries | `$100` (256) |
| completion entries | `$200` (512), via `IORING_SETUP_CQSIZE` |
| records, so operations in flight | `$100` (256) |
| tickets per group | `$40` (64) |
| `AIO:GROUP` definitions per image | `$10` (16) |

A submission with no free record, or with no room left in the submission ring,
is `E-AIO-FULL`. A record is claimed free-to-submitted with one `atomic-cas`;
every other record access runs under the module's one `TASK:FACILITY`, and the
locked bodies answer a code rather than throwing, so a refusal never leaves the
facility held.

## Errors

`E-AIO-SETUP`, `E-AIO-ENTER`, `E-AIO-STATE`, `E-AIO-FULL`, `E-AIO-BUSY`,
`E-AIO-GROUP`, `E-AIO-BOUNDS`, the `-9290..-9299` block of `lib/errors.f`.
`E-AIO-BOUNDS` is a transfer count past its allocation or below zero, a file
offset below `-1`, or a socket address length that is not positive.

## Kernel and target floor

- The ring interface and the readiness operations - `IORING_OP_POLL_ADD`,
  `IORING_OP_TIMEOUT`, `IORING_OP_ASYNC_CANCEL`, `IORING_OP_LINK_TIMEOUT` -
  date from Linux 5.5, and so do `IORING_OP_ACCEPT` and `IORING_OP_CONNECT`;
  `IORING_OP_READ` and `IORING_OP_WRITE` date from 5.6. The floor this module is
  written against is **5.10**, the first long-term kernel with all of them and
  with `IORING_FEAT_NODROP`.
  An older kernel refuses `io_uring_setup` with `ENOSYS` and the error names it.
  An offset of `-1` on a `READ` or a `WRITE` - use and advance the descriptor's
  own position - is io_uring's own rule for those two operations; it is measured
  in `lib/aio-test.f` on the kernel the suite runs on and not against the floor.
  A `POLL-ADD` with a deadline of zero links a zero-length timeout, and it still
  answers `ready` for a descriptor that is already ready because the kernel
  serves the poll inline before the linked timer is armed: measured 200 of 200
  each way on the running kernel and pinned by the zero-timeout questions of
  `lib/net/tcp4-test.f` and `lib/net/udp4-test.f`, not guaranteed by the floor.
- aarch64 only so far. `syscall` is a variadic C function, and on aarch64 a
  variadic call passes integer arguments in the ordinary registers, so the two
  declarations above are exact. **On x86-64 the caller of a variadic function
  must set `al` to the number of vector registers used** - zero here - so the
  x86-64 FFI has to zero `al` before the two declarations can be trusted on
  that target. That seam is not open in this module.

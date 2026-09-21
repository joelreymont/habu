# Asynchronous I/O on io_uring

`lib/aio.f` (package `AIO`) lets a task wait for a descriptor, a duration or a
cancellation without parking its thread inside the host call. Linux only.

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
words. A submission with no loop running is `E-AIO-STATE`.

## The surface

| Word | Effect | Blocks |
| --- | --- | --- |
| `AIO:LOOP-START` | `( -- )` | never; a second start is `E-AIO-STATE` |
| `AIO:LOOP-STOP` | `( -- )` | until the loop task has ended |
| `AIO:READABLE` | `( -- n )` | never; `POLLIN` |
| `AIO:WRITABLE` | `( -- n )` | never; `POLLOUT` |
| `AIO:POLL-ADD` | `( fd n ms -- AIO:ticket )` | never; `ms` -1 for no deadline |
| `AIO:TIMEOUT` | `( ms -- AIO:ticket )` | never |
| `AIO:CANCEL` | `( AIO:ticket -- )` | never |
| `AIO:AWAIT` | `( AIO:ticket -- AIO:outcome )` | until that operation has ended |
| `AIO:GROUP` | `( -- )` | defines a set of at most `$40` tickets |
| `AIO:GROUP+` | `( AIO:ticket AIO:group -- )` | never |
| `AIO:GROUP-` | `( AIO:ticket AIO:group -- )` | never |
| `AIO:GROUP-COUNT` | `( AIO:group -- n )` | never |
| `AIO:AWAIT-ANY` | `( AIO:group -- AIO:ticket AIO:outcome )` | until one of them has ended |

`AIO:outcome` is a layout, so no caller reads a result without deciding what to
do about every arm:

| arm | when |
| --- | --- |
| `ready ( n )` | the operation succeeded; for a poll, the revents mask |
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

## Waiting, and the hint protocol

`AIO:AWAIT` is a `TASK:STOP` loop, which means it obeys the rule
[threads.md](threads.md) states for one: **WAKE is a hint, STOP waits for a
hint, and the caller re-checks its own state after every STOP**. The await
re-reads the record and stops again when the loop woke it for something else,
and it calls `TASK:PAUSE` in the same loop, so a `TASK:HALT` ends a task parked
in an await.

The main thread has no TCB and parks on package `TASK`'s one main record, so a
program with no tasks of its own can await too.

**A task that submits gives up its own `TASK:AT-EXIT`.** At its first
submission `AIO` registers a cleanup on the calling task, and `TASK:AT-EXIT`
holds one quotation per task, so a cleanup the task registers afterwards
replaces `AIO`'s and one it registered before is replaced by it. The cleanup
cancels and forgets that task's operations in flight, which is what keeps the
loop from waking a TCB whose memory the join has released; a task that both
submits and needs a cleanup of its own has to call that cleanup from its body.

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
`E-AIO-GROUP`, the `-9290..-9299` block of `lib/errors.f`.

## Kernel and target floor

- The ring interface and the four operations used here - `IORING_OP_POLL_ADD`,
  `IORING_OP_TIMEOUT`, `IORING_OP_ASYNC_CANCEL`, `IORING_OP_LINK_TIMEOUT` -
  date from Linux 5.5. The floor this module is written against is **5.10**,
  the first long-term kernel with all of them and with `IORING_FEAT_NODROP`.
  An older kernel refuses `io_uring_setup` with `ENOSYS` and the error names it.
- aarch64 only so far. `syscall` is a variadic C function, and on aarch64 a
  variadic call passes integer arguments in the ordinary registers, so the two
  declarations above are exact. **On x86-64 the caller of a variadic function
  must set `al` to the number of vector registers used** - zero here - so the
  x86-64 FFI has to zero `al` before the two declarations can be trusted on
  that target. That seam is not open in this module.

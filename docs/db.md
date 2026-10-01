# Database access

`lib/pg.f` owns package `PG`: PostgreSQL over libpq, bound through the FFI
`FUNCTION:` declarer. It is the first half of the decision in
[database-models.md](database-models.md) — SQL through the FFI for hosted
programs, the polyFORTH record kit later for targets.

## The handles

A connection and a result are nominal cell families, `PG:connection` and
`PG:result`. Their converters stay private, so no caller can fabricate one; a
handle comes from starting a connection or completing an operation through PG.

Behind each handle is a slot in the module's registry holding the libpq
pointer, the owning task and a generation. Every entry point resolves the
handle through that registry first, so the four refusals below are decided
before any foreign call:

| Presented handle | Refusal |
| --- | --- |
| a connection that was closed, or one an image restore invalidated | `PG:E-HANDLE` |
| a handle belonging to another task | `PG:E-HANDLE` |
| a result that was already cleared | `PG:E-CLEARED` |
| a row or column outside the result | `PG:E-COLUMN` |

Retiring a slot invalidates its generation; a newly allocated handle receives
a process-wide generation that survives registry release. That makes the
result owner linear: `PG:CLEAR` is
the single consumption point, and a second `PG:CLEAR` — or any read — through
the same handle is `PG:E-CLEARED` rather than a use of a freed `PGresult`.
`PG:CLOSE` is the same contract for a connection, and it clears whatever
results that connection still owns first, so libpq is left holding nothing.

## Concurrency

Before opening a connection, declare the process's capacity with
`PG:CONFIGURE ( connections results parameters -- )`. For example,
`20 64 32 PG:CONFIGURE` reserves twenty connections, sixty-four live results
and thirty-two parameters per call. The registries use mapped typed buffers;
there is no fixed eight-connection table. Repeating the same declaration is
safe, including from another task; a different declaration is refused until
image preparation releases the registry. Configure again in the new process.

`CONNECT-START`, `SEND` and `POLL` let one task advance multiple connections.
The caller owns the readiness loop and operation deadlines. `CONNECT`, `EXEC`
and the other convenience words compose the same operations with `AWAIT`,
which waits through AIO. For those conveniences, start `AIO:START` before
connecting and stop it after database work drains. Habu currently parks the
calling task's pthread in `AIO:AWAIT`; only the progress interface lets a
dispatcher keep servicing other connections on that task.

The rule from [database-models.md](database-models.md) is one owner per
connection and results that never cross tasks. The registry enforces it: connection startup
records the calling task and every later word refuses a handle presented by any
other task with `PG:E-HANDLE`. A task that needs two databases opens two
connections; two tasks never share one. The parameter list, the call arena and
the diagnostic buffers all belong to the connection, so two tasks working on
their own connections never touch the same storage.

Handles do not survive image capture: the registry retires every slot and
closes its libpq resources and releases every call arena and registry buffer
when `IMAGE-LIFECYCLE` prepares an image, because a
restored image runs in another process where the libpq pointers are gone and
the mappings are not its own. A surviving handle then refuses instead of
reaching a freed address. Capture runs after the program's tasks stop, and the
AIO loop the convenience words wait through is one of them: call `AIO:STOP`
before the capture and `AIO:START` after it. `IMAGE-LIFECYCLE:PREPARE` with the
loop running ends the process with `task: activated task at capture`
([threads.md](threads.md)).

## Vocabulary

```forth
PG:CONFIGURE        ( n n n -- )  \ connection, result and parameter capacities
PG:CONNECT          ( ptr u8 n -- PG:connect-result )
PG:CLOSE            ( PG:connection -- )

PG:CONNECT-START    ( ptr u8 n -- PG:connection )
PG:SEND             ( PG:connection ptr u8 n -- )
PG:SEND-SCRIPT      ( PG:connection ptr u8 n -- )
PG:SEND-PREPARE     ( PG:connection ptr u8 n ptr u8 n -- )
PG:SEND-PREPARED    ( PG:connection ptr u8 n -- )
PG:POLL             ( PG:connection -- PG:progress )
PG:AWAIT            ( PG:connection -- PG:progress )

PG:PARAMS           ( PG:connection -- )
PG:TEXT+            ( PG:connection ptr u8 n -- )
PG:INT+             ( PG:connection n -- )
PG:NULL+            ( PG:connection -- )

PG:EXEC             ( PG:connection ptr u8 n -- PG:result )
PG:SCRIPT           ( PG:connection ptr u8 n -- PG:result )
PG:PREPARE          ( PG:connection ptr u8 n ptr u8 n -- PG:result )
PG:EXEC-PREPARED    ( PG:connection ptr u8 n -- PG:result )
PG:WITH-TRANSACTION ( PG:connection [ PG:connection -- PG:connection ] -- )

PG:OUTCOME          ( PG:result -- PG:outcome )
PG:ROWS             ( PG:result -- count )
PG:COLS             ( PG:result -- count )
PG:AFFECTED         ( PG:result -- count )
PG:NAME$            ( PG:result PG:col -- ptr u8 n )
PG:TEXT$            ( PG:result PG:row PG:col -- ptr u8 n )
PG:NULL?            ( PG:result PG:row PG:col -- bool )
PG:INT              ( PG:result PG:row PG:col -- n )
PG:CLEAR            ( PG:result -- )

PG:>ROW  ( n -- PG:row )   PG:ROW>N ( PG:row -- n )
PG:>COL  ( n -- PG:col )   PG:COL>N ( PG:col -- n )
```

`PG:row` and `PG:col` are distinct nominals, so a transposed
`PG:TEXT$` argument pair is a checker rejection rather than a wrong cell.

### Dispatcher progress

```forth
SUMTYPE progress 0
   VARIANT waiting fd n ;VARIANT           \ descriptor, AIO readiness mask
   VARIANT connected connection ;VARIANT
   VARIANT completed result ;VARIANT
   VARIANT refused ptr u8 n ;VARIANT       \ connection attempt failed
;SUMTYPE
```

Start with `CONNECT-START`, then call `POLL`. A `waiting` result supplies the
descriptor and `AIO:READABLE`/`AIO:WRITABLE` mask to register in the caller's
loop. Call `POLL` again once that readiness arrives; use the newly returned
descriptor each time, since libpq may replace it while connecting. The first
step also supplies the writable readiness libpq requires before its first
connection poll. `connected` permits a `SEND` operation; `completed` hands
the result to the caller for `OUTCOME`, reading and `CLEAR`. These words do
not start an AIO loop or wait inside PG.

Only one operation may be pending on a connection. Parameter changes and a
second send are refused with `E-STATEMENT` until the pending operation ends.
Sends copy their statement and parameters before returning, so callers may
reuse their input storage. `CLOSE` can abandon a connecting or busy handle;
it also releases the pending result reservation. On a transport failure PG
closes that connection and throws `E-EXEC`. A SQL rejection instead completes
with a result whose `OUTCOME` carries the server diagnostic.

Use a Unix socket or numeric `hostaddr` to avoid DNS resolution blocking
connection startup. The dispatcher must enforce connection deadlines itself:
libpq ignores `connect_timeout` during asynchronous connection polling.
These are libpq's [connection polling requirements](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PQCONNECTSTARTPARAMS).

### Outcomes

```forth
SUMTYPE connect-result 0
   VARIANT connected connection ;VARIANT
   VARIANT refused ptr u8 n ;VARIANT        \ libpq's own message
;SUMTYPE

SUMTYPE outcome 0
   VARIANT ok ;VARIANT                      \ a command with no rows
   VARIANT rows ;VARIANT                    \ a result set to read
   VARIANT failed ptr u8 n ptr u8 n ;VARIANT \ SQLSTATE, primary server message
;SUMTYPE
```

No error is ever a raw `n`: a server refusal arrives as the `failed` arm
carrying the five SQLSTATE bytes and the primary message, and everything the
module itself refuses is one of the named throws below. A result that carries
no primary message of its own — a connection that died under the query — falls
back to libpq's connection message, so the `failed` arm is never empty. An
empty statement, whose `PGRES_EMPTY_QUERY` result has neither a SQLSTATE nor a
message, is refused as `PG:E-STATEMENT` before it reaches the server.

The `failed` spans are copies held by the owning connection, so they outlive
`PG:CLEAR` and are replaced by the next `PG:OUTCOME` on that connection. The
`refused` span of a failed `PG:CONNECT` is held by the slot the attempt used
and stays readable until the next `PG:CONNECT`. A span from `PG:TEXT$` or
`PG:NAME$` is libpq's own memory inside the result and is valid until
`PG:CLEAR`; copy it if it must outlive the result.

### Parameters

Parameters are text format throughout. `PG:PARAMS` empties the connection's
list and `PG:TEXT+`, `PG:INT+` and `PG:NULL+` append to it in `$1`, `$2`, …
order; `PG:INT+` renders the whole signed 64-bit range as decimal text.
`PG:EXEC` and `PG:EXEC-PREPARED` consume the list and leave it empty, so no
call can inherit another call's parameters.

libpq is therefore given a NULL `paramTypes` (the server infers each type), a
NULL `paramLengths` (ignored for text) and a NULL `paramFormats` (a NULL array
means every parameter is text). Only `paramValues` is built here, as a cell
array of NUL-terminated C strings in the connection's call arena; a NULL
parameter is a NULL entry in that array, which is exactly how libpq spells it.

**Statement and parameter text have no fixed ceiling — they are bounded by
memory only.** Each connection builds one call in an arena it allocates from
`lib/memory.f`, sized to that call: the statement bytes, every parameter with
its NUL terminator, and the `paramValues` pointer array. Parameters are
recorded as offsets, so growing the arena may move it without invalidating
anything already staged. The arena is released the instant libpq returns — it
has copied everything into its own message by then — and it belongs to the
connection, so no path leaks it: the next `PG:PARAMS`, `PG:CLOSE` and image
capture all release it too.

Counts are the application's `PG:CONFIGURE` declaration, each with
`PG:E-CAPACITY` at the boundary. `BEGIN`, `COMMIT` and `ROLLBACK` are fixed
statements and run from a small per-connection buffer, which is why they leave
a pending parameter list untouched.

### Scripts

`PG:SCRIPT` runs a whole script — a migration file, several statements in one
text — through libpq's simple-query protocol, which is the only protocol that
takes more than one statement: `PG:EXEC` and `PG:EXEC-PREPARED` ride the
extended protocol, where a second command is SQLSTATE `42601`. Use `PG:SCRIPT`
for scripts and migrations; `PG:EXEC` stays the default for everything else,
because it is the one that takes parameters and it runs exactly one statement.

libpq answers with the LAST statement's result, and a failing statement
abandons the rest, so the same `PG:outcome` covers a script unchanged — a
failure carries the failing statement's SQLSTATE and message. Measured:
PostgreSQL runs a multi-statement simple query as ONE implicit transaction, so
a statement that fails rolls the earlier ones back with it; a script is atomic
even outside `PG:WITH-TRANSACTION`. The protocol carries no parameters at all,
so a pending parameter list is a caller error and is refused with
`PG:E-STATEMENT` rather than silently dropped, and so is an empty text.

### Transactions

`PG:WITH-TRANSACTION` runs `BEGIN`, then the quotation, then `COMMIT`; a throw
inside the quotation runs `ROLLBACK` and reaches the caller with its original
code. The quotation takes the connection and returns it, which is what lets it
cross the `catch` boundary. A `PG:WITH-TRANSACTION` inside another one on the
same connection is `PG:E-TRANSACTION`, because a nested `BEGIN` is a no-op in
PostgreSQL and the inner `COMMIT` would end the outer transaction.

The three transaction verbs leave the connection's parameter list exactly as
they found it. That matters because a quotation may not read the caller's
locals: building the parameters before `PG:WITH-TRANSACTION` and spending them
inside the body is the only way to carry a caller's values in, and the worked
example below does it.

## Throws

| Code | Meaning |
| --- | --- |
| `PG:E-CONNECT` | libpq could not build a connection at all |
| `PG:E-EXEC` | a transport/readiness failure, no result, or a rejected transaction verb |
| `PG:E-COLUMN` | a row or column index outside the result |
| `PG:E-TYPE` | a column read as a type its bytes are not, including `PG:INT` of NULL |
| `PG:E-CLEARED` | a result used after `PG:CLEAR`, or cleared a second time |
| `PG:E-TRANSACTION` | a `PG:WITH-TRANSACTION` inside another one |
| `PG:E-HANDLE` | a handle another task owns, or one an image restore invalidated |
| `PG:E-CAPACITY` | more live connections, results or parameters than the module stores |
| `PG:E-PLATFORM` | this module is qualified on Linux only, and refuses a foreign target before it binds |
| `PG:E-STATEMENT` | an empty statement/name, script parameters, or an operation invalid for the connection's current phase |

The block is `-9250..-9259`, minted in `lib/pg.f`, which owns package PG.

## Worked example

```forth
require lib/pg.f

package REPORT
using PG

: LOAD ( PG:connection -- PG:connection )
   dup s" insert into note (tender, body) values ($1, $2)" EXEC CLEAR ;

: STORE ( PG:connection ptr u8 n -- ) {: c a u :}
   c PARAMS
   c 42 INT+
   c a u TEXT+
   c [: LOAD ;] WITH-TRANSACTION ;

: SHOW ( PG:connection -- ) {: c :}
   c PARAMS
   c 42 INT+
   c s" select id, body from note where tender = $1 order by id" EXEC {: r :}
   r OUTCOME MATCH PG:outcome
      ok OF ENDOF
      rows OF
         r ROWS COUNT>N 0 ?do
            r i >ROW 0 >COL INT .
            r i >ROW 1 >COL TEXT$ type cr
         loop
      ENDOF
      failed OF type cr type cr ENDOF
   ;MATCH
   r CLEAR ;

;using
;package
```

`WITH-TRANSACTION` here builds the parameter list before the quotation runs,
because the quotation may not read the caller's locals; the connection carries
the list into `EXEC`.

## Tests

`lib/pg-test.f` runs against a live server, and the native gate runs it as the
`pg` row through `test/db/pg-cluster.f`. The harness makes a private
trust-authentication cluster with `initdb`, starts `postgres` on it as its own
child, listening on a Unix-domain socket only (`listen_addresses=''`), and runs
these cases and then [`lib/db/rows-test.f`](#its-test), each in a child engine
with the cluster's conninfo (`host=<socket directory> dbname=postgres
user=habu`) as its one script argument. It then stops the server whatever the
cases' outcome, and the directories it made are removed when it exits:

```sh
bin/hb --load test/db/pg-cluster.f
```

Case files named after `--` run in place of those two.

Each step has a deadline: `initdb`, the server's start and its stop 60 seconds
each, and each case file 60, 300 seconds in all with the two default files,
inside the row's 360 ([gate.md](gate.md)). A step past its deadline is ended
as a failed one is, and the server, if it runs, is stopped; then the harness
ends with an uncaught `E-PROC-TIMEOUT`, which the engine reports on stderr as
`hb: uncaught throw code -2502` before it exits 67. A gate pool reads that as
a deadline missed on a loaded host, `kind=TIMEOUT-UNDER-LOAD`, and not as a
defect. Any other failure exits 1; a missing `initdb` or `postgres` exits 67
naming it. The first step to fail decides: a server whose stop fails after a
case passed its deadline leaves the row a timeout, and a stop past its
deadline after a case failed leaves it a failure.

A server a row starts is the row's child for as long as it runs, and so is
every process it forks, so the pool's kill reaches all of them. The harness
therefore runs `postgres` itself: `pg_ctl` forks the server, calls `setsid`,
starts it through a shell and exits, which leaves the server with init for a
parent in a session of its own, beyond the pool's tree walk
([gate.md](gate.md)). The harness waits for the status line of
`postmaster.pid` to read `ready`, as `pg_ctl -w` does, and stops the server
with SIGQUIT, `pg_ctl -m immediate`'s signal.

The server has to end through its own exit, because that is the only thing
that removes its System V shared-memory segment. PostgreSQL keys the segment by
the data directory's inode and removes it on its way out; a server that is
SIGKILLed leaves it for good, since APFS does not hand the inode out again and
nothing else removes it, and `kern.sysv.shmmni` caps the segments host-wide (32
on the macOS hosts here). A host out of segments starts no PostgreSQL at all.
`initdb`'s own backend makes the same segment for each step `initdb` runs and
removes it the same way.

So the harness catches SIGTERM, SIGINT and SIGHUP. On one while `initdb` runs,
it sends `initdb` SIGTERM, which `initdb` catches: it exits once the step it is
in has ended, and that step's backend has removed the segment. On one once the
server runs, it sends the server its SIGQUIT and ends the running case engine
with every process under it; an immediate shutdown SIGKILLs the server's
children still alive after five seconds and then exits. Either is given six
seconds. An `initdb` step or a server still running after them is killed with
every process under it and leaves its segment, to be removed as below. Then
the harness removes its directories and dies of the signal. A pool sends a
row's root SIGTERM before it kills the row's tree when the root catches it,
and gives it ten seconds ([gate.md](gate.md)), so a pg row killed at its
deadline, or because the gate root was signalled, leaves no process and no
directory, and no segment unless an `initdb` step or a server outlived its six
seconds. The same row then runs `test/db/pg-kill-test.f`, which has a
pool kill the harness both of those ways while a case holds a backend busy,
and checks that no process of the row, no segment and no socket directory is
left. A signal during `initdb` is not part of that test, which would need an
`initdb` slowed past the grace; `initdb` here takes about 1.2 seconds in all.

SIGKILL cannot be answered. After one to the harness, or to a gate root
(whose rows' reapers then SIGKILL each row's group, the harness included), the
server keeps running with init for a parent and both directories stay. Recover
with `kill -QUIT` to the pid on the first line of `<root>/data/postmaster.pid`,
then remove the two directories; the server's command line names both (`-D`
and `-k`). A server that was SIGKILLed as well has left its segment: `ipcs -m
-p` lists it with `NATTCH` 0 and a creator pid that no longer runs, and
`ipcrm -m <id>` removes it. Line 7 of a `postmaster.pid` that still exists
names its segment as `<key> <id>`.

A SIGKILL to the harness while `initdb` runs leaves `initdb` running with init
for a parent. `initdb` leads a process group of its own, so a reaper's SIGKILL
of the row's group misses it as well. It writes to `<root>/initdb.log`, not to
the harness. It runs to its end and logs `Success.`, and each step's backend
removes its segment as it exits, so no segment is left and no server starts.
Both directories stay: `<root>`, holding `initdb.log` and a complete `data`
that no server has used, and the empty socket directory (a `habu-pg-*` under
`TMPDIR` for a harness run alone, the row's `HB_SOCK_TMP` under a pool). Wait
for `initdb` to end, or send it SIGTERM, after which it ends with its current
step and removes `data` itself; its command line names `<root>/data` (`-D`).
Then remove the two directories.

With no TCP listener, rows running beside each other cannot collide on a port.
The data directory is under the row's `HB_TMP`. The socket directory is the
row's `HB_SOCK_TMP`, the short directory the pool makes for each child it
spawns under `TMPDIR` ([gate.md](gate.md)), because a pool slot's `HB_TMP` is
already about 100 bytes long and postgres refuses a socket path over 103
(`Unix-domain socket path ... is too long (maximum 103 bytes)` on macOS, whose
`sun_path` holds 104). Run on its own, the harness makes one under `TMPDIR`.

The dispatcher case uses two real connections from one task: a query waits on
an advisory lock, the same task releases it through the other connection, and
the first query completes. It also closes a pending query and then fills the
declared result registry, checking that cancellation did not leak a slot.

The server binaries are a gate requirement on every host
([bootstrap.md](bootstrap.md#requirements)): without `initdb` or `postgres` on
`PATH` the row fails with `pg-cluster: required executable missing on PATH:`
and the name. Loaded without its argument, `lib/pg-test.f` dies naming the
harness. Neither skips. `test/five-bindings.f` also loads and binds package
`PG` beside the other foreign libraries.

## Row readers (package DB-ROWS)

`lib/db/rows.f` opens connections and reads rows. A span a `COL-` reader
answers is copied into the reading task's own arena, so it stays valid after
`PG:CLEAR` and until that task's next read (`READ-RESET`, which `BY-ID` runs).
The arena is per task because static storage is one copy for the whole image
while a server reads from many tasks at once: with one arena, a second task's
read copies its bytes over a record the first task still holds.

Two declarations come before the first `OPEN` or read:

```forth
3 DB-ROWS:CONNECTIONS+          \ each module, for the connections it opens
8 DB-ROWS:CONFIGURE-READERS     \ once: the tasks that read rows, main included
```

`CONNECTIONS+` adds to one connection the process entry owns; the first `OPEN`
calls `PG:CONFIGURE` with that count, 64 results and 32 parameters, and a later
`CONNECTIONS+` is `DB-ROWS:E-CAPACITY`. `CONFIGURE-READERS` behaves like
`PG:CONFIGURE`: the same count again is harmless, a different one is
`E-CAPACITY` until image preparation releases the arenas, and a read before it
or from one task more than the count is `DB-ROWS:E-READERS`. A task takes its
arena on its first read and keeps it; each arena holds 16 KiB per read, and a
read past that is `E-CAPACITY`.

```forth
DB-ROWS:OPEN-START   ( ptr u8 n -- PG:connection )  \ for a caller driving PG:POLL
DB-ROWS:OPEN         ( ptr u8 n -- PG:connection )  \ E-CONNECT on refusal; needs AIO:START
DB-ROWS:OPEN-ENV     ( -- PG:connection )           \ libpq's PG* environment
DB-ROWS:CONNECTIONS  ( -- n )

DB-ROWS:READ-RESET    ( -- )
DB-ROWS:BY-ID         ( PG:connection n ptr u8 n -- PG:result )  \ $1 = n; one row or E-ROW
DB-ROWS:WITH-ROW      ( PG:result [ PG:result -- R ] -- R )      \ clears on every path
DB-ROWS:ROWS-OR-THROW ( PG:result -- PG:result )  \ E-QUERY unless rows
DB-ROWS:FIRST-ROW     ( PG:result -- PG:result )  \ E-ROW on no row

DB-ROWS:COL-TEXT  ( PG:result n -- ptr u8 n )       \ NULL is E-ROW
DB-ROWS:COL-TEXT? ( PG:result n -- ptr u8 n bool )  \ NULL is empty and false
DB-ROWS:COL-INT   ( PG:result n -- n )              \ NULL is PG:E-TYPE
DB-ROWS:COL-ID    ( PG:result n -- n )              \ NULL is 0
DB-ROWS:COL-REAL  ( PG:result n -- r )              \ NULL is 0.0
DB-ROWS:COL-BOOL  ( PG:result n -- bool )           \ NULL is E-ROW

DB-ROWS:AT$       ( PG:result n n -- ptr u8 n )     \ row, column; libpq's bytes
DB-ROWS:AT?$      ( PG:result n n -- ptr u8 n bool )
DB-ROWS:AT-INT    ( PG:result n n -- n )
DB-ROWS:AT-BOOL   ( PG:result n n -- bool )
DB-ROWS:ONE-INT   ( PG:result -- n )                \ clears; NULL or no row is E-ROW
```

The `COL-` readers read the first row; the `AT` readers read any row in place
and answer libpq's bytes, valid until `PG:CLEAR`. A decoder run through
`WITH-ROW` returns a record whose spans outlive the result.

DB-ROWS holds `-9320..-9324` of the block `-9320..-9329`, minted in
`lib/db/rows.f`, which owns package DB-ROWS.

### Its test

`lib/db/rows-test.f` runs in the `pg` row: `test/db/pg-cluster.f` runs it after
`lib/pg-test.f`, against the same cluster and with the same conninfo as its one
script argument. The first file that fails ends the run, and the row's last
line is `pg-cluster: <file> failed`. Loaded without the argument, the file dies
naming the harness; there is no skip.

It reads records whose spans must outlive `PG:CLEAR`, every NULL arm and
refusal (each repeated past the 64 results a leak would exhaust), two worker
tasks holding records at once, and a third worker one reader past the
declaration.

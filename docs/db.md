# Database access

`lib/pg.f` owns package `PG`: PostgreSQL over libpq, bound through the FFI
`FUNCTION:` declarer. It is the first half of the decision in
[database-models.md](database-models.md) — SQL through the FFI for hosted
programs, the polyFORTH record kit later for targets.

## The handles

A connection and a result are nominal cell families, `PG:connection` and
`PG:result`. Their converters stay private, so no caller can fabricate one; a
handle is only ever what `PG:CONNECT`, `PG:EXEC`, `PG:PREPARE` or
`PG:EXEC-PREPARED` returned.

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

`CONNECT` and the query words use libpq's nonblocking send/poll operations and
wait for socket readiness through the running AIO loop. Start `AIO:START`
before the first connection and stop it after database work has drained.
Habu currently parks the calling task's pthread in `AIO:AWAIT`: these convenience
words do not provide thread-free suspension.

The rule from [database-models.md](database-models.md) is one connection per
task and results that never cross tasks. The registry enforces it: `CONNECT`
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
reaching a freed address.

## Vocabulary

```forth
PG:CONFIGURE        ( n n n -- )  \ connection, result and parameter capacities
PG:CONNECT          ( ptr u8 n -- PG:connect-result )
PG:CLOSE            ( PG:connection -- )

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
| `PG:E-EXEC` | libpq returned no result, or a transaction verb the server rejected |
| `PG:E-COLUMN` | a row or column index outside the result |
| `PG:E-TYPE` | a column read as a type its bytes are not, including `PG:INT` of NULL |
| `PG:E-CLEARED` | a result used after `PG:CLEAR`, or cleared a second time |
| `PG:E-TRANSACTION` | a `PG:WITH-TRANSACTION` inside another one |
| `PG:E-HANDLE` | a handle another task owns, or one an image restore invalidated |
| `PG:E-CAPACITY` | more live connections, results or parameters than the module stores |
| `PG:E-PLATFORM` | `libpq.so.5` is not the shared-library name this target loads |
| `PG:E-STATEMENT` | an empty statement text or prepared-statement name, or a `PG:SCRIPT` with parameters pending |

The block is `-9250..-9259` in `lib/errors.f`.

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

`lib/pg-test.f` runs against a live server. `test/db/pg-fixture.sh` starts a
throwaway trust-authentication cluster on a free loopback port in a temporary
directory, exports `HABU_PG_CONNINFO`, runs the command and then stops and
removes the cluster:

```sh
test/db/pg-fixture.sh build/hb-pg --load lib/pg-test.f
```

Without `HABU_PG_CONNINFO` the test prints `pg-test: skipped, HABU_PG_CONNINFO
names no server` and asserts nothing. It is registered in
`test/gate-stdlib-cases.f` as suite `pg` on exactly those terms: the gate
has no PostgreSQL, so every gate run certifies package `PG` and prints the
skip, and the fixture is how the module is exercised for real.

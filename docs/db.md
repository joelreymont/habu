# Database access

`lib/db/pq.f` owns package `DB`: PostgreSQL over libpq, bound through the FFI
`FUNCTION:` declarer. It is the first half of the decision in
[database-models.md](database-models.md) — SQL through the FFI for hosted
programs, the polyFORTH record kit later for targets.

## The handles

A connection and a result are nominal cell families, `DB:connection` and
`DB:result`. Their converters stay private, so no caller can fabricate one; a
handle is only ever what `DB:CONNECT`, `DB:EXEC`, `DB:PREPARE` or
`DB:EXEC-PREPARED` returned.

Behind each handle is a slot in the module's registry holding the libpq
pointer, the owning task and a generation. Every entry point resolves the
handle through that registry first, so the four refusals below are decided
before any foreign call:

| Presented handle | Refusal |
| --- | --- |
| a connection that was closed, or one an image restore invalidated | `DB:E-HANDLE` |
| a handle belonging to another task | `DB:E-HANDLE` |
| a result that was already cleared | `DB:E-CLEARED` |
| a row or column outside the result | `DB:E-COLUMN` |

Retiring a slot bumps its generation, so every handle minted from it stops
resolving at once. That is what makes the result owner linear: `DB:CLEAR` is
the single consumption point, and a second `DB:CLEAR` — or any read — through
the same handle is `DB:E-CLEARED` rather than a use of a freed `PGresult`.
`DB:CLOSE` is the same contract for a connection, and it clears whatever
results that connection still owns first, so libpq is left holding nothing.

## Concurrency

The rule from [database-models.md](database-models.md) is one connection per
task and results that never cross tasks. The registry enforces it: `CONNECT`
records the calling task and every later word refuses a handle presented by any
other task with `DB:E-HANDLE`. A task that needs two databases opens two
connections; two tasks never share one. The parameter list, the call arena and
the diagnostic buffers all belong to the connection, so two tasks working on
their own connections never touch the same storage.

Handles do not survive image capture: the registry retires every slot and
releases every call arena when `IMAGE-LIFECYCLE` prepares an image, because a
restored image runs in another process where the libpq pointers are gone and
the mappings are not its own. A surviving handle then refuses instead of
reaching a freed address.

## Vocabulary

```forth
DB:CONNECT          ( ptr u8 n -- DB:connect-result )
DB:CLOSE            ( DB:connection -- )

DB:PARAMS           ( DB:connection -- )
DB:TEXT+            ( DB:connection ptr u8 n -- )
DB:INT+             ( DB:connection n -- )
DB:NULL+            ( DB:connection -- )

DB:EXEC             ( DB:connection ptr u8 n -- DB:result )
DB:PREPARE          ( DB:connection ptr u8 n ptr u8 n -- DB:result )
DB:EXEC-PREPARED    ( DB:connection ptr u8 n -- DB:result )
DB:WITH-TRANSACTION ( DB:connection [ DB:connection -- DB:connection ] -- )

DB:OUTCOME          ( DB:result -- DB:outcome )
DB:ROWS             ( DB:result -- count )
DB:COLS             ( DB:result -- count )
DB:AFFECTED         ( DB:result -- count )
DB:NAME$            ( DB:result DB:col -- ptr u8 n )
DB:TEXT$            ( DB:result DB:row DB:col -- ptr u8 n )
DB:NULL?            ( DB:result DB:row DB:col -- bool )
DB:INT              ( DB:result DB:row DB:col -- n )
DB:CLEAR            ( DB:result -- )

DB:>ROW  ( n -- DB:row )   DB:ROW>N ( DB:row -- n )
DB:>COL  ( n -- DB:col )   DB:COL>N ( DB:col -- n )
```

`DB:row` and `DB:col` are distinct nominals, so a transposed
`DB:TEXT$` argument pair is a checker rejection rather than a wrong cell.

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
message, is refused as `DB:E-STATEMENT` before it reaches the server.

The `failed` spans are copies held by the owning connection, so they outlive
`DB:CLEAR` and are replaced by the next `DB:OUTCOME` on that connection. The
`refused` span of a failed `DB:CONNECT` is held by the slot the attempt used
and stays readable until the next `DB:CONNECT`. A span from `DB:TEXT$` or
`DB:NAME$` is libpq's own memory inside the result and is valid until
`DB:CLEAR`; copy it if it must outlive the result.

### Parameters

Parameters are text format throughout. `DB:PARAMS` empties the connection's
list and `DB:TEXT+`, `DB:INT+` and `DB:NULL+` append to it in `$1`, `$2`, …
order; `DB:INT+` renders the whole signed 64-bit range as decimal text.
`DB:EXEC` and `DB:EXEC-PREPARED` consume the list and leave it empty, so no
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
connection, so no path leaks it: the next `DB:PARAMS`, `DB:CLOSE` and image
capture all release it too.

What remains bounded is counts, not sizes: 32 parameters per call, 8
connections and 32 live results in the process, each `DB:E-CAPACITY` at the
boundary. `BEGIN`, `COMMIT` and `ROLLBACK` are this module's own fixed
statements and run from a small per-connection buffer, which is why they leave
a pending parameter list untouched.

### Transactions

`DB:WITH-TRANSACTION` runs `BEGIN`, then the quotation, then `COMMIT`; a throw
inside the quotation runs `ROLLBACK` and reaches the caller with its original
code. The quotation takes the connection and returns it, which is what lets it
cross the `catch` boundary. A `DB:WITH-TRANSACTION` inside another one on the
same connection is `DB:E-TRANSACTION`, because a nested `BEGIN` is a no-op in
PostgreSQL and the inner `COMMIT` would end the outer transaction.

The three transaction verbs leave the connection's parameter list exactly as
they found it. That matters because a quotation may not read the caller's
locals: building the parameters before `DB:WITH-TRANSACTION` and spending them
inside the body is the only way to carry a caller's values in, and the worked
example below does it.

## Throws

| Code | Meaning |
| --- | --- |
| `DB:E-CONNECT` | libpq could not build a connection at all |
| `DB:E-EXEC` | libpq returned no result, or a transaction verb the server rejected |
| `DB:E-COLUMN` | a row or column index outside the result |
| `DB:E-TYPE` | a column read as a type its bytes are not, including `DB:INT` of NULL |
| `DB:E-CLEARED` | a result used after `DB:CLEAR`, or cleared a second time |
| `DB:E-TRANSACTION` | a `DB:WITH-TRANSACTION` inside another one |
| `DB:E-HANDLE` | a handle another task owns, or one an image restore invalidated |
| `DB:E-CAPACITY` | more live connections, results or parameters than the module stores |
| `DB:E-PLATFORM` | `libpq.so.5` is not the shared-library name this target loads |
| `DB:E-STATEMENT` | an empty statement text or prepared-statement name |

The block is `-9250..-9259` in `lib/errors.f`.

## Worked example

```forth
require lib/db/pq.f

package REPORT
using DB

: LOAD ( DB:connection -- DB:connection )
   dup s" insert into note (tender, body) values ($1, $2)" EXEC CLEAR ;

: STORE ( DB:connection ptr u8 n -- ) {: c a u :}
   c PARAMS
   c 42 INT+
   c a u TEXT+
   c [: LOAD ;] WITH-TRANSACTION ;

: SHOW ( DB:connection -- ) {: c :}
   c PARAMS
   c 42 INT+
   c s" select id, body from note where tender = $1 order by id" EXEC {: r :}
   r OUTCOME MATCH DB:outcome
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

`lib/db/pq-test.f` runs against a live server. `test/db/pg-fixture.sh` starts a
throwaway trust-authentication cluster on a free loopback port in a temporary
directory, exports `HABU_PG_CONNINFO`, runs the command and then stops and
removes the cluster:

```sh
test/db/pg-fixture.sh build/hb-pq --load lib/db/pq-test.f
```

Without `HABU_PG_CONNINFO` the test prints `pq-test: skipped, HABU_PG_CONNINFO
names no server` and asserts nothing. It is registered in
`test/gate-stdlib-cases.f` as suite `db-pq` on exactly those terms: the gate
has no PostgreSQL, so every gate run certifies package `DB` and prints the
skip, and the fixture is how the module is exercised for real.

\ process-command-test.f - focused tests for src/lib/process-command.f.
\ Run: bin/hb --load src/lib/process-command-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/task.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-command.f
require src/lib/test/outcome.f
require src/lib/test/wait.f

package CMD-TEST
private

create ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c, 10 c,
create ENTRY-OUT 101 c, 110 c, 116 c, 114 c, 121 c, 10 c, 10 c, 10 c,

5000 constant HB-TIMEOUT-MS
1000 constant CMD-TIMEOUT-MS
50 constant SHORT-TIMEOUT-MS
20000000000 constant WAIT-NS             \ a task row's whole wait: 20 s

: RESULT-CODE ( result<n,n> -- n )   \ 0 on clean exit, else the completion code
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

: RUN# ( CMD:command ptr u8 n n -- CMD:command n )
   >MS CMD:RUN-RC RESULT-CODE ;

: CHECK-LINE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: out:ptr outu:n expected:ptr expectedu:n :}
   outu expectedu 1+ < if
      outu expectedu 1+ T=
      out outu exit
   then
   out expectedu expected expectedu T$=
   out expectedu + c@ 10 T=
   out expectedu 1+ + outu expectedu 1+ - ;

: LF-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 u 0 ?do a i + c@ 10 = if 1 + then loop ;

\ ---- the kept rows, each inside its own scope ------------------------------------
: PRINTF ( CMD:command -- CMD:command )
   s" %s:%s" CMD:ARG+
   s" left" CMD:ARG+
   s" right" CMD:ARG+
   s" /usr/bin/printf" CMD-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ s" left:right" T$=
   CMD:ERR$ nip 0 T=
   PROC-ARGV-N @ COUNT>N 0 T=            \ the scope writes no process-wide staging
   PROC-ENV-N @ COUNT>N 0 T= ;

: STDIN ( CMD:command -- CMD:command )
   s" cmd-stdin" CMD:IN!
   s" /bin/cat" CMD-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ s" cmd-stdin" T$=
   CMD:ERR$ nip 0 T= ;

: HERMETIC-ENV ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   s" test/process-env-child.f" CMD:ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" CMD:ENV+
   s" bin/hb" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ ENV-OUT 8 T$=
   CMD:ERR$ nip 0 T= ;

: ENTRY-ENV ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   s" test/process-env-child.f" CMD:ARG+
   s" HABU_PROC_ENV_TEST=entry" CMD:ENV-ENTRY+
   s" bin/hb" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ ENTRY-OUT 8 T$=
   CMD:ERR$ nip 0 T= ;

: INHERITED-ENV ( CMD:command -- CMD:command )
   s" test/process-env-child.f" CMD:ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" CMD:ENV+
   s" bin/hb" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$
   s" alpha" CHECK-LINE
   s" HOME" GETENV CHECK-LINE
   s" PATH" GETENV CHECK-LINE
   nip 0 T= ;

\ Both arms of the result<n,n>: a clean exit MATCHes ok carrying 0, a nonzero
\ exit MATCHes err carrying the code, and RC@ answers the stored outcome again.
: RC-ARMS ( CMD:command -- CMD:command )
   s" /usr/bin/true" CMD-TIMEOUT-MS >MS CMD:RUN-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   CMD:RESET
   s" /usr/bin/false" CMD-TIMEOUT-MS >MS CMD:RUN-RC
   MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH
   CMD:RC@ MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH ;

\ A deadline is no completion code: RC@ and RUN-RC throw E-PROC-TIMEOUT for
\ it, where RUN-OUTCOME and OUTCOME@ keep it as data.
: SLEEP ( CMD:command -- CMD:command outcome )
   s" 5" CMD:ARG+
   s" /bin/sleep" SHORT-TIMEOUT-MS >MS CMD:RUN-OUTCOME ;

: TIMEOUT ( CMD:command -- CMD:command )
   SLEEP T-OUTCOME-TIMEOUT
   CMD:OUTCOME@ T-OUTCOME-TIMEOUT
   CMD:OUT$ nip 0 T=
   CMD:ERR$ nip 0 T= ;

: TIMEOUT-RC@ ( CMD:command -- CMD:command )
   SLEEP drop CMD:RC@ drop ;

: TIMEOUT-RUN-RC ( CMD:command -- CMD:command )
   s" 5" CMD:ARG+
   s" /bin/sleep" SHORT-TIMEOUT-MS RUN# drop ;

\ CMD:RUN: a clean exit returns with the command still usable; an exit of 3
\ throws the caller's code with the child's stderr on this process's stderr, so
\ that row runs in a child hb (test/process-command-run-child.f); a deadline
\ throws E-PROC-TIMEOUT.
: RUN-OK ( CMD:command -- CMD:command )
   s" run-ok" CMD:ARG+
   s" /usr/bin/printf" CMD-TIMEOUT-MS >MS 7 CMD:RUN
   CMD:OUT$ s" run-ok" T$= ;

: RUN-FAILS ( CMD:command -- CMD:command )
   s" test/process-command-run-child.f" CMD:ARG+
   s" bin/hb" HB-TIMEOUT-MS RUN# {: rc:n :}
   CMD:ERR$ s" child-err" T$=
   CMD:OUT$ s" test: ok" CHECK-LINE nip 0 T=
   rc 0 T= ;

: RUN-TIMEOUT ( CMD:command -- CMD:command )
   s" 5" CMD:ARG+
   s" /bin/sleep" 1 >MS 7 CMD:RUN ;

\ A run that fails leaves its code, not the clean exit of an earlier run:
\ OUTCOME@ and RC@ throw it again. The catch sits inside the scope, the shape dot
\ habu-settle-stale-linear-0bcc904c questions.
: SPAWN-FAILS ( CMD:command -- CMD:command )
   s" /usr/bin/true" CMD-TIMEOUT-MS RUN# 0 T=
   [: s" /nonexistent-pcmdt-path" CMD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop ;]
   catch E-PROC-SPAWN T=
   [: CMD:OUTCOME@ drop ;] catch E-PROC-SPAWN T=
   [: CMD:RC@ drop ;] catch E-PROC-SPAWN T= ;

\ A deadline or path refused before the run starts throws its own code and keeps
\ the previous run's outcome and capture.
: EXIT3$ ( -- ptr u8 n )  s" printf x; exit 3" ;

: EXIT3-KEPT ( CMD:command -- CMD:command )
   CMD:OUTCOME@ {: oc :}
   CMD:OUT$ {: out:ptr outu:n :}
   CMD:ERR$ {: err:ptr erru:n :}
   EXIT3$ out outu err erru oc 3 T-OUTCOME-EXITED=
   out outu s" x" T$= ;

: REFUSED-KEEPS ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   EXIT3$ CMD:ARG+
   s" /bin/sh" CMD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop
   [: s" /bin/sh" -1 >MS CMD:RUN-OUTCOME drop ;] catch E-PROC-TIMEOUT T=
   EXIT3-KEPT
   [: s" " CMD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop ;] catch E-PROC-OUTPUT T=
   EXIT3-KEPT ;

\ CWD!: the child runs from the given directory, proved by a relative-path
\ effect (cat of a file that exists only there), so a symlinked or slash-ended
\ temp base cannot fake a mismatch; the directory is registered for cleanup
\ before the run so a throw does not leak it. A missing directory is refused
\ before any spawn.
create DIR PATH-CAP allot   variable DIR-U
create REL PATH-CAP allot   variable REL-U
: CWD-BODY$ ( -- ptr u8 n )  s" from the private directory" ;

: CWD-RUN ( CMD:command -- CMD:command )
   DIR DIR-U @ CMD:CWD!
   s" here.txt" CMD:ARG+
   s" /bin/cat" CMD-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ CWD-BODY$ T$= ;

: CWD ( -- )
   FS:CLEANUP-RESET
   s" hb-cmd-cwd" FS:MKTEMP-DIR {: d:ptr du :}
   d du FS:CLEANUP-TREE+
   du PATH-CAP <= TTRUE
   du PATH-CAP > if FS:CLEANUP-RUN exit then
   d DIR du BYTE-COPY  du DIR-U !
   DIR DIR-U @ s" here.txt" REL FS:JOIN-PATH REL-U !
   REL REL-U @ CWD-BODY$ FS:WRITE-ALL
   [: CWD-RUN ;] CMD:WITH-COMMAND
   s" here.txt" FS:FILE? TFALSE
   FS:CLEANUP-RUN ;

: CWD-MISSING ( CMD:command -- CMD:command )
   s" /nonexistent-hb-cmd-dir" CMD:CWD! ;

: BAD-ENV-NAME ( CMD:command -- CMD:command )
   s" BAD=NAME" s" x" CMD:ENV+ ;

: BAD-ENV-ENTRY ( CMD:command -- CMD:command )
   s" MISSING_EQUALS" CMD:ENV-ENTRY+ ;

\ ---- growth: no member has a ceiling ------------------------------------------------
\ Both streams past any fixed buffer the old context had, captured whole.
100000 constant OUT-BYTES
70000 constant ERR-BYTES

: BOTH-STREAMS ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" head -c 100000 /dev/zero; head -c 70000 /dev/zero >&2" CMD:ARG+
   s" /bin/sh" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ nip OUT-BYTES T=
   CMD:ERR$ nip ERR-BYTES T= ;

\ One byte past 128 KiB of patterned stdin round-trips through cat.
131073 constant IN-BYTES
create IN IN-BYTES allot

: IN-FILL ( -- )
   IN-BYTES 0 do i 251 mod IN i + c! loop ;

: BIG-STDIN ( CMD:command -- CMD:command )
   IN IN-BYTES CMD:IN!
   s" /bin/cat" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ IN IN-BYTES T$= ;

\ 300 arguments, each printed on its own line.
300 constant ARGS

: MANY-ARGS ( CMD:command -- CMD:command )
   s\" %s\n" CMD:ARG+
   ARGS 0 do s" a" CMD:ARG+ loop
   s" /usr/bin/printf" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ {: a:ptr u:n :}
   u ARGS 2 * T=
   a u LF-COUNT ARGS T= ;

\ 3000 distinct rows reach a hermetic child, which prints exactly them.
3000 constant ROWS
create NAME 5 allot                      \ V and four digits

: NAME! ( n -- ) {: k:n :}
   86 NAME c!
   k 1000 / 10 mod 48 + NAME 1 + c!
   k 100 / 10 mod 48 + NAME 2 + c!
   k 10 / 10 mod 48 + NAME 3 + c!
   k 10 mod 48 + NAME 4 + c! ;

: MANY-ROWS ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   ROWS 0 do
      i NAME!
      NAME 5 s" x" CMD:ENV+
   loop
   s" /usr/bin/env" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ {: a:ptr u:n :}
   u ROWS 8 * T=                         \ "Vdddd=x\n" per row
   a u LF-COUNT ROWS T= ;

\ ---- nesting: an inner scope is its own command ---------------------------------
: SAY ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   a u CMD:ARG+
   s" /usr/bin/printf" CMD-TIMEOUT-MS RUN# 0 T= ;

: INNER ( CMD:command CMD:command -- CMD:command CMD:command )
   s" inner" SAY
   CMD:OUT$ s" inner" T$=
   swap
   CMD:OUT$ s" outer" T$=
   CMD:RESET
   s" again" SAY
   CMD:OUT$ s" again" T$=
   swap ;

: NESTED ( CMD:command -- CMD:command )
   s" outer" SAY
   [: INNER ;] CMD:WITH-COMMAND
   CMD:OUT$ s" again" T$= ;

\ ---- freed on throw ---------------------------------------------------------------
\ Each cycle captures 4 MiB and throws out of the scope. Kept, 64 cycles would add
\ 256 MiB to the peak resident set; released, the peak stays near one cycle's.
0 constant RUSAGE-SELF
$20 constant MAXRSS-OFF                  \ ru_maxrss, past ru_utime and ru_stime
$90 constant RUSAGE-BYTES                \ struct rusage: two timevals and fourteen longs
4194304 constant FAT-BYTES
64 constant FAT-CYCLES
create RUSAGE-BUF RUSAGE-BYTES allot
1 constant MS-ASYNC                     \ the same flag on macOS and Linux

PROCESS-SYMBOLS

FUNCTION: RESOURCE-USAGE getrusage ( n ptr u8 -- i32 )
   1 RUSAGE-BYTES WRITES-BYTES
;FUNCTION

FUNCTION: SYNC-RANGE msync ( n n n -- i32 ) ;FUNCTION

: LE64@ ( ptr u8 -- n ) {: source :}
   0 8 0 do source 7 i - + c@ swap 8 lshift or loop ;

: PEAK-RSS ( -- n )
   RUSAGE-SELF RUSAGE-BUF RESOURCE-USAGE 0 <> if E-PROC-OUTPUT throw then
   RUSAGE-BUF MAXRSS-OFF + LE64@ ;

\ 64 MiB of ru_maxrss: bytes on macOS, kilobytes on Linux.
: RSS-BOUND ( -- n )
   HB-TARGET-MACOS? if 67108864 else 65536 then ;

: FAT-RUN ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" 4194304" CMD:ARG+
   s" /dev/zero" CMD:ARG+
   s" /usr/bin/head" HB-TIMEOUT-MS RUN# 0 T=
   CMD:OUT$ nip FAT-BYTES T= ;

: FAT ( CMD:command -- CMD:command )
   FAT-RUN
   E-PROC-CANCELLED throw ;

: FAT-ONCE ( -- )
   [: [: FAT ;] CMD:WITH-COMMAND ;] E-PROC-CANCELLED TTHROWSQ ;

: FREED-ON-THROW ( -- )
   FAT-ONCE
   PEAK-RSS {: before:n :}
   FAT-CYCLES 0 do FAT-ONCE loop
   PEAK-RSS before - RSS-BOUND < TTRUE ;

\ ---- two tasks, each with its own scope ---------------------------------------
\ Both children sleep before writing, so the two captures overlap. A task asserts
\ nothing: it records, and this thread checks after the join.
100000 constant TASK-BYTES
variable A-RC   variable A-LEN   variable A-BAD
variable B-RC   variable B-LEN   variable B-BAD
TASK:MIN-STACK TASK:TASK TASK-A
TASK:MIN-STACK TASK:TASK TASK-B

\ Bytes that break `yes ch`'s pattern: ch at even offsets, LF at odd ones.
: BAD-YES ( ptr u8 n n -- n ) {: a:ptr u:n ch:n :}
   0 u 0 ?do i 1 and 0<> if 10 else ch then a i + c@ <> if 1 + then loop ;

: YES ( CMD:command ptr u8 n -- CMD:command n ) {: a:ptr u:n :}
   s" -c" CMD:ARG+
   a u CMD:ARG+
   s" /bin/sh" HB-TIMEOUT-MS RUN# ;

: A-RUN ( CMD:command -- CMD:command )
   s" sleep 0.3; yes A | head -c 100000" YES A-RC !
   CMD:OUT$ {: a:ptr u:n :}
   u A-LEN !
   a u 65 BAD-YES A-BAD ! ;

: B-RUN ( CMD:command -- CMD:command )
   s" sleep 0.3; yes B | head -c 100000" YES B-RC !
   CMD:OUT$ {: a:ptr u:n :}
   u B-LEN !
   a u 66 BAD-YES B-BAD ! ;

: A-BODY ( -- ) [: A-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;
: B-BODY ( -- ) [: B-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;

: BOTH-DONE? ( -- bool )
   TASK-A TASK:DONE? TASK-B TASK:DONE? and ;

: JOIN-0 ( ptr n -- )
   TASK:JOIN MATCH result ok OF 0 T= ENDOF err OF 0 T= ENDOF ;MATCH ;

: TWO-TASKS ( -- )
   ['] A-BODY TASK-A TASK:ACTIVATE
   ['] B-BODY TASK-B TASK:ACTIVATE
   [: BOTH-DONE? ;] mono-ns WAIT-NS + s" process-command-test two tasks"
   TEST-WAIT:POLL
   TASK-A JOIN-0
   TASK-B JOIN-0
   A-RC @ 0 T=   A-LEN @ TASK-BYTES T=   A-BAD @ 0 T=
   B-RC @ 0 T=   B-LEN @ TASK-BYTES T=   B-BAD @ 0 T= ;

\ ---- a default added between another task's prepare and its spawn -------------
\ An inheriting run keeps the default rows' addresses from its prepare to its
\ spawn. This thread holds the spawn lock, so the worker stops between the two:
\ its capture reset, right after the prepare, sets its RC cell to -1 before the
\ capture's descriptors wait for the lock. A default added then, larger than a
\ row chunk, must leave the prepared rows where they were, and the child sees
\ the environment as prepared.
$21000 constant LATE-U
create LATE LATE-U allot
variable LATE-RC
variable LATE-FIRST                      \ 1 when the prepared default is the first line
variable LATE-SEEN                       \ 1 when the late default reached the child
TASK:MIN-STACK TASK:TASK LATE-TASK

: LATE-RUN ( CMD:command -- CMD:command )
   s" /usr/bin/env" CMD-TIMEOUT-MS RUN# LATE-RC !
   CMD:OUT$ {: a:ptr u:n :}
   S\" HABU_CMD_DEFAULT=orig\n" {: first:ptr firstu:n :}
   0 LATE-FIRST !
   u firstu >= if a firstu first firstu STR= if 1 LATE-FIRST ! then then
   0 LATE-SEEN !
   a u s" HABU_CMD_LATE=" CONTAINS? if 1 LATE-SEEN ! then ;

: LATE-BODY ( -- ) [: LATE-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;

: LATE-PREPARED? ( -- bool )
   LATE-TASK PROC-STORAGE TASK:HIS BYTE-VIEW PROC-RC-OFF + CELL-VIEW @ -1 = ;

: LATE-WINDOW ( -- )
   ['] LATE-BODY LATE-TASK TASK:ACTIVATE
   [: LATE-PREPARED? ;] mono-ns WAIT-NS + s" process-command-test prepared run"
   TEST-WAIT:POLL
   s" HABU_CMD_LATE" >LEN LATE LATE-U >LEN PROC-ENV-DEFAULT+ ;

: LATE-DONE? ( -- bool ) LATE-TASK TASK:DONE? ;

: LATE-DEFAULT ( -- )
   LATE-U 0 ?do [char] x LATE i + c! loop
   s" HABU_CMD_DEFAULT" >LEN s" orig" >LEN PROC-ENV-DEFAULT+
   [: LATE-WINDOW ;] FD-LOCK:WITH
   [: LATE-DONE? ;] mono-ns WAIT-NS + s" process-command-test late default"
   TEST-WAIT:POLL
   LATE-TASK JOIN-0
   PROC-ENV-DEFAULT-RESET
   LATE-RC @ 0 T=
   LATE-FIRST @ 1 T=
   LATE-SEEN @ 0 T= ;

\ ---- a halted task's open scope is released -----------------------------------
\ The worker parks inside its scope with 1 MiB captured. The halt unwinds the task
\ at TASK:PAUSE through the scope's `finally`; the capture's mapping must be gone
\ once the task has ended, which msync reports as -1 for an unmapped address
\ (macOS mincore answers 0 there, so it cannot tell).
1048576 constant PARK-BYTES
4096 constant PAGE-PROBE
variable PARK-RC
variable PARK-LEN
variable PARK-ADDR
variable PARK-READY
TASK:MIN-STACK TASK:TASK PARKER

: MAPPED ( n -- n )
   PAGE-PROBE MS-ASYNC SYNC-RANGE ;

: PARK ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" 1048576" CMD:ARG+
   s" /dev/zero" CMD:ARG+
   s" /usr/bin/head" HB-TIMEOUT-MS RUN# PARK-RC !
   CMD:OUT$ {: a:ptr u:n :}
   u PARK-LEN !
   a NULL-PTR BYTE-VIEW - PARK-ADDR !
   1 PARK-READY atomic!
   begin TASK:PAUSE again ;

: PARK-BODY ( -- ) [: PARK ;] CMD:WITH-COMMAND ;

: PARKED? ( -- bool ) PARK-READY atomic@ 0<> ;
: PARKER-DONE? ( -- bool ) PARKER TASK:DONE? ;

: HALTED-TASK ( -- )
   0 PARK-READY !
   ['] PARK-BODY PARKER TASK:ACTIVATE
   [: PARKED? ;] mono-ns WAIT-NS + s" process-command-test parked task"
   TEST-WAIT:POLL
   PARK-RC @ 0 T=
   PARK-LEN @ PARK-BYTES T=
   PARK-ADDR @ MAPPED 0 T=
   PARKER TASK:HALT
   [: PARKER-DONE? ;] mono-ns WAIT-NS + s" process-command-test halted task"
   TEST-WAIT:POLL
   PARKER TASK:JOIN MATCH result ok OF drop 1 0 T= ENDOF err OF E-TASK-NO-RESULT T= ENDOF ;MATCH
   PARK-ADDR @ MAPPED -1 T= ;

\ ---- a runaway producer meets its deadline -------------------------------------
\ `yes` writes until the deadline kills it. The run answers the timeout outcome
\ with the capture grown past a page, and the capture's mapping is gone once the
\ scope ends.
variable YES-ADDR

: RUNAWAY ( CMD:command -- CMD:command )
   s" /usr/bin/yes" SHORT-TIMEOUT-MS >MS CMD:RUN-OUTCOME T-OUTCOME-TIMEOUT
   CMD:OUT$ {: a:ptr u:n :}
   u PAGE-PROBE > TTRUE
   a NULL-PTR BYTE-VIEW - YES-ADDR !
   YES-ADDR @ MAPPED 0 T= ;

: RUNAWAY-FREED ( -- )
   [: RUNAWAY ;] CMD:WITH-COMMAND
   YES-ADDR @ MAPPED -1 T= ;

\ ---- a swapped token is refused ------------------------------------------------
\ The inner body swaps and hands back the outer token. The inner scope refuses it
\ with CMD:E-HANDLE and closes its own context; the throw leaves the outer scope,
\ which closes its own, before the outer body can run on a closed context. Both
\ scopes' capture mappings are gone after the refusal, and a fresh scope then runs
\ as usual.
variable OUTER-ADDR
variable INNER-ADDR

: FAT-ADDR ( CMD:command -- CMD:command n )
   FAT-RUN
   CMD:OUT$ drop NULL-PTR BYTE-VIEW -
   dup MAPPED 0 T= ;

: SWAP-INNER ( CMD:command CMD:command -- CMD:command CMD:command )
   FAT-ADDR INNER-ADDR ! swap ;

: SWAP-OUTER ( CMD:command -- CMD:command )
   FAT-ADDR OUTER-ADDR !
   [: SWAP-INNER ;] CMD:WITH-COMMAND
   s" unreached" SAY ;

: FRESH-RUN ( CMD:command -- CMD:command )
   s" fresh" SAY
   CMD:OUT$ s" fresh" T$= ;

: SWAPPED ( -- )
   [: [: SWAP-OUTER ;] CMD:WITH-COMMAND ;] CMD:E-HANDLE TTHROWSQ
   OUTER-ADDR @ MAPPED -1 T=
   INNER-ADDR @ MAPPED -1 T=
   [: FRESH-RUN ;] CMD:WITH-COMMAND ;

public

: MAIN ( -- )
   T-RESET
   FREED-ON-THROW
   SWAPPED
   [: PRINTF ;] CMD:WITH-COMMAND
   [: STDIN ;] CMD:WITH-COMMAND
   [: HERMETIC-ENV ;] CMD:WITH-COMMAND
   [: ENTRY-ENV ;] CMD:WITH-COMMAND
   [: INHERITED-ENV ;] CMD:WITH-COMMAND
   [: RC-ARMS ;] CMD:WITH-COMMAND
   [: TIMEOUT ;] CMD:WITH-COMMAND
   [: [: TIMEOUT-RC@ ;] CMD:WITH-COMMAND ;] E-PROC-TIMEOUT TTHROWSQ
   [: [: TIMEOUT-RUN-RC ;] CMD:WITH-COMMAND ;] E-PROC-TIMEOUT TTHROWSQ
   [: RUN-OK ;] CMD:WITH-COMMAND
   [: RUN-FAILS ;] CMD:WITH-COMMAND
   [: [: RUN-TIMEOUT ;] CMD:WITH-COMMAND ;] E-PROC-TIMEOUT TTHROWSQ
   [: SPAWN-FAILS ;] CMD:WITH-COMMAND
   [: REFUSED-KEEPS ;] CMD:WITH-COMMAND
   CWD
   [: [: CWD-MISSING ;] CMD:WITH-COMMAND ;] E-PROC-PATH TTHROWSQ
   [: [: BAD-ENV-NAME ;] CMD:WITH-COMMAND ;] E-PROC-ENV TTHROWSQ
   [: [: BAD-ENV-ENTRY ;] CMD:WITH-COMMAND ;] E-PROC-ENV TTHROWSQ
   [: BOTH-STREAMS ;] CMD:WITH-COMMAND
   IN-FILL
   [: BIG-STDIN ;] CMD:WITH-COMMAND
   [: MANY-ARGS ;] CMD:WITH-COMMAND
   [: MANY-ROWS ;] CMD:WITH-COMMAND
   [: NESTED ;] CMD:WITH-COMMAND
   TWO-TASKS
   LATE-DEFAULT
   HALTED-TASK
   RUNAWAY-FREED
   T-REPORT
   s" process-command-test: ok" type cr ;

;package

CMD-TEST:MAIN

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

create PCMDT-ENV-OUT 97 c, 108 c, 112 c, 104 c, 97 c, 10 c, 10 c, 10 c,
create PCMDT-ENTRY-OUT 101 c, 110 c, 116 c, 114 c, 121 c, 10 c, 10 c, 10 c,

5000 constant PCMDT-HB-TIMEOUT-MS
1000 constant PCMDT-CMD-TIMEOUT-MS
50 constant PCMDT-SHORT-TIMEOUT-MS
20000000000 constant PCMDT-WAIT-NS       \ a task row's whole wait: 20 s

: PCMDT-RC>N ( result<n,n> -- n )   \ 0 on clean exit, else the completion code
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

: PCMDT-RUN# ( CMD:command ptr u8 n n -- CMD:command n )
   >MS CMD:RUN-RC PCMDT-RC>N ;

: PCMDT-CHECK-LINE ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: out:ptr outu:n expected:ptr expectedu:n :}
   outu expectedu 1+ < if
      outu expectedu 1+ T=
      out outu exit
   then
   out expectedu expected expectedu T$=
   out expectedu + c@ 10 T=
   out expectedu 1+ + outu expectedu 1+ - ;

: PCMDT-LF-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 u 0 ?do a i + c@ 10 = if 1 + then loop ;

\ ---- the kept rows, each inside its own scope ------------------------------------
: PCMDT-PRINTF ( CMD:command -- CMD:command )
   s" %s:%s" CMD:ARG+
   s" left" CMD:ARG+
   s" right" CMD:ARG+
   s" /usr/bin/printf" PCMDT-CMD-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ s" left:right" T$=
   CMD:ERR$ nip 0 T=
   PROC-ARGV-N @ COUNT>N 0 T=            \ the scope writes no process-wide staging
   PROC-ENV-N @ COUNT>N 0 T= ;

: PCMDT-STDIN ( CMD:command -- CMD:command )
   s" cmd-stdin" CMD:IN!
   s" /bin/cat" PCMDT-CMD-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ s" cmd-stdin" T$=
   CMD:ERR$ nip 0 T= ;

: PCMDT-HERMETIC-ENV ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   s" test/process-env-child.f" CMD:ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" CMD:ENV+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ PCMDT-ENV-OUT 8 T$=
   CMD:ERR$ nip 0 T= ;

: PCMDT-ENTRY-ENV ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   s" test/process-env-child.f" CMD:ARG+
   s" HABU_PROC_ENV_TEST=entry" CMD:ENV-ENTRY+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ PCMDT-ENTRY-OUT 8 T$=
   CMD:ERR$ nip 0 T= ;

: PCMDT-INHERITED-ENV ( CMD:command -- CMD:command )
   s" test/process-env-child.f" CMD:ARG+
   s" HABU_PROC_ENV_TEST" s" alpha" CMD:ENV+
   s" bin/hb" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$
   s" alpha" PCMDT-CHECK-LINE
   s" HOME" GETENV PCMDT-CHECK-LINE
   s" PATH" GETENV PCMDT-CHECK-LINE
   nip 0 T= ;

\ Both arms of the result<n,n>: a clean exit MATCHes ok carrying 0, a nonzero
\ exit MATCHes err carrying the code, and RC@ answers the stored outcome again.
: PCMDT-RC-ARMS ( CMD:command -- CMD:command )
   s" /usr/bin/true" PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   CMD:RESET
   s" /usr/bin/false" PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-RC
   MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH
   CMD:RC@ MATCH result ok OF drop 1 0 T= ENDOF err OF 1 T= ENDOF ;MATCH ;

\ A deadline is no completion code: RC@ and RUN-RC throw E-PROC-TIMEOUT for
\ it, where RUN-OUTCOME and OUTCOME@ keep it as data.
: PCMDT-SLEEP ( CMD:command -- CMD:command outcome )
   s" 5" CMD:ARG+
   s" /bin/sleep" PCMDT-SHORT-TIMEOUT-MS >MS CMD:RUN-OUTCOME ;

: PCMDT-TIMEOUT ( CMD:command -- CMD:command )
   PCMDT-SLEEP T-OUTCOME-TIMEOUT
   CMD:OUTCOME@ T-OUTCOME-TIMEOUT
   CMD:OUT$ nip 0 T=
   CMD:ERR$ nip 0 T= ;

: PCMDT-TIMEOUT-RC@ ( CMD:command -- CMD:command )
   PCMDT-SLEEP drop CMD:RC@ drop ;

: PCMDT-TIMEOUT-RUN-RC ( CMD:command -- CMD:command )
   s" 5" CMD:ARG+
   s" /bin/sleep" PCMDT-SHORT-TIMEOUT-MS PCMDT-RUN# drop ;

\ A run that fails leaves its code, not the clean exit of an earlier run:
\ OUTCOME@ and RC@ throw it again. The catch sits inside the scope, the shape dot
\ habu-settle-stale-linear-0bcc904c questions.
: PCMDT-SPAWN-FAILS ( CMD:command -- CMD:command )
   s" /usr/bin/true" PCMDT-CMD-TIMEOUT-MS PCMDT-RUN# 0 T=
   [: s" /nonexistent-pcmdt-path" PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop ;]
   catch E-PROC-SPAWN T=
   [: CMD:OUTCOME@ drop ;] catch E-PROC-SPAWN T=
   [: CMD:RC@ drop ;] catch E-PROC-SPAWN T= ;

\ A deadline or path refused before the run starts throws its own code and keeps
\ the previous run's outcome and capture.
: PCMDT-EXIT3$ ( -- ptr u8 n )  s" printf x; exit 3" ;

: PCMDT-EXIT3-KEPT ( CMD:command -- CMD:command )
   CMD:OUTCOME@ {: oc :}
   CMD:OUT$ {: out:ptr outu:n :}
   CMD:ERR$ {: err:ptr erru:n :}
   PCMDT-EXIT3$ out outu err erru oc 3 T-OUTCOME-EXITED=
   out outu s" x" T$= ;

: PCMDT-REFUSED-KEEPS ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   PCMDT-EXIT3$ CMD:ARG+
   s" /bin/sh" PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop
   [: s" /bin/sh" -1 >MS CMD:RUN-OUTCOME drop ;] catch E-PROC-TIMEOUT T=
   PCMDT-EXIT3-KEPT
   [: s" " PCMDT-CMD-TIMEOUT-MS >MS CMD:RUN-OUTCOME drop ;] catch E-PROC-OUTPUT T=
   PCMDT-EXIT3-KEPT ;

\ CWD!: the child runs from the given directory, proved by a relative-path
\ effect (cat of a file that exists only there), so a symlinked or slash-ended
\ temp base cannot fake a mismatch; the directory is registered for cleanup
\ before the run so a throw does not leak it. A missing directory is refused
\ before any spawn.
create PCMDT-DIR FS-PATH-CAP allot   variable PCMDT-DIR-U
create PCMDT-REL FS-PATH-CAP allot   variable PCMDT-REL-U
: PCMDT-CWD-BODY$ ( -- ptr u8 n )  s" from the private directory" ;

: PCMDT-CWD-RUN ( CMD:command -- CMD:command )
   PCMDT-DIR PCMDT-DIR-U @ CMD:CWD!
   s" here.txt" CMD:ARG+
   s" /bin/cat" PCMDT-CMD-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ PCMDT-CWD-BODY$ T$= ;

: PCMDT-CWD ( -- )
   CLEANUP-RESET
   s" hb-cmd-cwd" TMPDIR-MKDIR {: d:ptr du :}
   d du CLEANUP-TREE+
   du FS-PATH-CAP <= TTRUE
   du FS-PATH-CAP > if CLEANUP-RUN exit then
   d PCMDT-DIR du BYTE-COPY  du PCMDT-DIR-U !
   PCMDT-DIR PCMDT-DIR-U @ s" here.txt" PCMDT-REL JOIN-PATH PCMDT-REL-U !
   PCMDT-REL PCMDT-REL-U @ PCMDT-CWD-BODY$ WRITE-ALL
   [: PCMDT-CWD-RUN ;] CMD:WITH-COMMAND
   s" here.txt" FILE? TFALSE
   CLEANUP-RUN ;

: PCMDT-CWD-MISSING ( CMD:command -- CMD:command )
   s" /nonexistent-hb-cmd-dir" CMD:CWD! ;

: PCMDT-BAD-ENV-NAME ( CMD:command -- CMD:command )
   s" BAD=NAME" s" x" CMD:ENV+ ;

: PCMDT-BAD-ENV-ENTRY ( CMD:command -- CMD:command )
   s" MISSING_EQUALS" CMD:ENV-ENTRY+ ;

\ ---- growth: no member has a ceiling ------------------------------------------------
\ Both streams past any fixed buffer the old context had, captured whole.
100000 constant PCMDT-OUT-BYTES
70000 constant PCMDT-ERR-BYTES

: PCMDT-BOTH-STREAMS ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" head -c 100000 /dev/zero; head -c 70000 /dev/zero >&2" CMD:ARG+
   s" /bin/sh" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ nip PCMDT-OUT-BYTES T=
   CMD:ERR$ nip PCMDT-ERR-BYTES T= ;

\ One byte past 128 KiB of patterned stdin round-trips through cat.
131073 constant PCMDT-IN-BYTES
create PCMDT-IN PCMDT-IN-BYTES allot

: PCMDT-IN-FILL ( -- )
   PCMDT-IN-BYTES 0 do i 251 mod PCMDT-IN i + c! loop ;

: PCMDT-BIG-STDIN ( CMD:command -- CMD:command )
   PCMDT-IN PCMDT-IN-BYTES CMD:IN!
   s" /bin/cat" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ PCMDT-IN PCMDT-IN-BYTES T$= ;

\ 300 arguments, each printed on its own line.
300 constant PCMDT-ARGS

: PCMDT-MANY-ARGS ( CMD:command -- CMD:command )
   s\" %s\n" CMD:ARG+
   PCMDT-ARGS 0 do s" a" CMD:ARG+ loop
   s" /usr/bin/printf" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ {: a:ptr u:n :}
   u PCMDT-ARGS 2 * T=
   a u PCMDT-LF-COUNT PCMDT-ARGS T= ;

\ 3000 distinct rows reach a hermetic child, which prints exactly them.
3000 constant PCMDT-ROWS
create PCMDT-NAME 5 allot                \ V and four digits

: PCMDT-NAME! ( n -- ) {: k:n :}
   86 PCMDT-NAME c!
   k 1000 / 10 mod 48 + PCMDT-NAME 1 + c!
   k 100 / 10 mod 48 + PCMDT-NAME 2 + c!
   k 10 / 10 mod 48 + PCMDT-NAME 3 + c!
   k 10 mod 48 + PCMDT-NAME 4 + c! ;

: PCMDT-MANY-ROWS ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   PCMDT-ROWS 0 do
      i PCMDT-NAME!
      PCMDT-NAME 5 s" x" CMD:ENV+
   loop
   s" /usr/bin/env" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ {: a:ptr u:n :}
   u PCMDT-ROWS 8 * T=                   \ "Vdddd=x\n" per row
   a u PCMDT-LF-COUNT PCMDT-ROWS T= ;

\ ---- nesting: an inner scope is its own command ---------------------------------
: PCMDT-SAY ( CMD:command ptr u8 n -- CMD:command ) {: a:ptr u:n :}
   a u CMD:ARG+
   s" /usr/bin/printf" PCMDT-CMD-TIMEOUT-MS PCMDT-RUN# 0 T= ;

: PCMDT-INNER ( CMD:command CMD:command -- CMD:command CMD:command )
   s" inner" PCMDT-SAY
   CMD:OUT$ s" inner" T$=
   swap
   CMD:OUT$ s" outer" T$=
   CMD:RESET
   s" again" PCMDT-SAY
   CMD:OUT$ s" again" T$=
   swap ;

: PCMDT-NESTED ( CMD:command -- CMD:command )
   s" outer" PCMDT-SAY
   [: PCMDT-INNER ;] CMD:WITH-COMMAND
   CMD:OUT$ s" again" T$= ;

\ ---- freed on throw ---------------------------------------------------------------
\ Each cycle captures 4 MiB and throws out of the scope. Kept, 64 cycles would add
\ 256 MiB to the peak resident set; released, the peak stays near one cycle's.
0 constant RUSAGE-SELF
$20 constant MAXRSS-OFF                  \ ru_maxrss, past ru_utime and ru_stime
$90 constant RUSAGE-BYTES                \ struct rusage: two timevals and fourteen longs
4194304 constant PCMDT-FAT-BYTES
64 constant PCMDT-FAT-CYCLES
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
: PCMDT-RSS-BOUND ( -- n )
   HB-TARGET-MACOS? if 67108864 else 65536 then ;

: PCMDT-FAT-RUN ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" 4194304" CMD:ARG+
   s" /dev/zero" CMD:ARG+
   s" /usr/bin/head" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# 0 T=
   CMD:OUT$ nip PCMDT-FAT-BYTES T= ;

: PCMDT-FAT ( CMD:command -- CMD:command )
   PCMDT-FAT-RUN
   E-PROC-CANCELLED throw ;

: PCMDT-FAT-ONCE ( -- )
   [: [: PCMDT-FAT ;] CMD:WITH-COMMAND ;] E-PROC-CANCELLED TTHROWSQ ;

: PCMDT-FREED-ON-THROW ( -- )
   PCMDT-FAT-ONCE
   PEAK-RSS {: before:n :}
   PCMDT-FAT-CYCLES 0 do PCMDT-FAT-ONCE loop
   PEAK-RSS before - PCMDT-RSS-BOUND < TTRUE ;

\ ---- two tasks, each with its own scope ---------------------------------------
\ Both children sleep before writing, so the two captures overlap. A task asserts
\ nothing: it records, and this thread checks after the join.
100000 constant PCMDT-TASK-BYTES
variable PCMDT-A-RC   variable PCMDT-A-LEN   variable PCMDT-A-BAD
variable PCMDT-B-RC   variable PCMDT-B-LEN   variable PCMDT-B-BAD
TASK:MIN-STACK TASK:TASK PCMDT-TASK-A
TASK:MIN-STACK TASK:TASK PCMDT-TASK-B

\ Bytes that break `yes ch`'s pattern: ch at even offsets, LF at odd ones.
: PCMDT-BAD-YES ( ptr u8 n n -- n ) {: a:ptr u:n ch:n :}
   0 u 0 ?do i 1 and 0<> if 10 else ch then a i + c@ <> if 1 + then loop ;

: PCMDT-YES ( CMD:command ptr u8 n -- CMD:command n ) {: a:ptr u:n :}
   s" -c" CMD:ARG+
   a u CMD:ARG+
   s" /bin/sh" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# ;

: PCMDT-A-RUN ( CMD:command -- CMD:command )
   s" sleep 0.3; yes A | head -c 100000" PCMDT-YES PCMDT-A-RC !
   CMD:OUT$ {: a:ptr u:n :}
   u PCMDT-A-LEN !
   a u 65 PCMDT-BAD-YES PCMDT-A-BAD ! ;

: PCMDT-B-RUN ( CMD:command -- CMD:command )
   s" sleep 0.3; yes B | head -c 100000" PCMDT-YES PCMDT-B-RC !
   CMD:OUT$ {: a:ptr u:n :}
   u PCMDT-B-LEN !
   a u 66 PCMDT-BAD-YES PCMDT-B-BAD ! ;

: PCMDT-A-BODY ( -- ) [: PCMDT-A-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;
: PCMDT-B-BODY ( -- ) [: PCMDT-B-RUN ;] CMD:WITH-COMMAND 0 TASK:RETURN ;

: PCMDT-BOTH-DONE? ( -- bool )
   PCMDT-TASK-A TASK:DONE? PCMDT-TASK-B TASK:DONE? and ;

: PCMDT-JOIN-0 ( ptr n -- )
   TASK:JOIN MATCH result ok OF 0 T= ENDOF err OF 0 T= ENDOF ;MATCH ;

: PCMDT-TWO-TASKS ( -- )
   ['] PCMDT-A-BODY PCMDT-TASK-A TASK:ACTIVATE
   ['] PCMDT-B-BODY PCMDT-TASK-B TASK:ACTIVATE
   [: PCMDT-BOTH-DONE? ;] mono-ns PCMDT-WAIT-NS + s" process-command-test two tasks"
   TEST-WAIT:POLL
   PCMDT-TASK-A PCMDT-JOIN-0
   PCMDT-TASK-B PCMDT-JOIN-0
   PCMDT-A-RC @ 0 T=   PCMDT-A-LEN @ PCMDT-TASK-BYTES T=   PCMDT-A-BAD @ 0 T=
   PCMDT-B-RC @ 0 T=   PCMDT-B-LEN @ PCMDT-TASK-BYTES T=   PCMDT-B-BAD @ 0 T= ;

\ ---- a halted task's open scope is released -----------------------------------
\ The worker parks inside its scope with 1 MiB captured. The halt unwinds the task
\ at TASK:PAUSE through the scope's `finally`; the capture's mapping must be gone
\ once the task has ended, which msync reports as -1 for an unmapped address
\ (macOS mincore answers 0 there, so it cannot tell).
1048576 constant PCMDT-PARK-BYTES
4096 constant PCMDT-PAGE-PROBE
variable PCMDT-PARK-RC
variable PCMDT-PARK-LEN
variable PCMDT-PARK-ADDR
variable PCMDT-PARK-READY
TASK:MIN-STACK TASK:TASK PCMDT-PARKER

: PCMDT-MAPPED ( n -- n )
   PCMDT-PAGE-PROBE MS-ASYNC SYNC-RANGE ;

: PCMDT-PARK ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" 1048576" CMD:ARG+
   s" /dev/zero" CMD:ARG+
   s" /usr/bin/head" PCMDT-HB-TIMEOUT-MS PCMDT-RUN# PCMDT-PARK-RC !
   CMD:OUT$ {: a:ptr u:n :}
   u PCMDT-PARK-LEN !
   a NULL-PTR BYTE-VIEW - PCMDT-PARK-ADDR !
   1 PCMDT-PARK-READY atomic!
   begin TASK:PAUSE again ;

: PCMDT-PARK-BODY ( -- ) [: PCMDT-PARK ;] CMD:WITH-COMMAND ;

: PCMDT-PARKED? ( -- bool ) PCMDT-PARK-READY atomic@ 0<> ;
: PCMDT-PARKER-DONE? ( -- bool ) PCMDT-PARKER TASK:DONE? ;

: PCMDT-HALTED-TASK ( -- )
   0 PCMDT-PARK-READY !
   ['] PCMDT-PARK-BODY PCMDT-PARKER TASK:ACTIVATE
   [: PCMDT-PARKED? ;] mono-ns PCMDT-WAIT-NS + s" process-command-test parked task"
   TEST-WAIT:POLL
   PCMDT-PARK-RC @ 0 T=
   PCMDT-PARK-LEN @ PCMDT-PARK-BYTES T=
   PCMDT-PARK-ADDR @ PCMDT-MAPPED 0 T=
   PCMDT-PARKER TASK:HALT
   [: PCMDT-PARKER-DONE? ;] mono-ns PCMDT-WAIT-NS + s" process-command-test halted task"
   TEST-WAIT:POLL
   PCMDT-PARKER TASK:JOIN MATCH result ok OF drop 1 0 T= ENDOF err OF E-TASK-NO-RESULT T= ENDOF ;MATCH
   PCMDT-PARK-ADDR @ PCMDT-MAPPED -1 T= ;

\ ---- a runaway producer meets its deadline -------------------------------------
\ `yes` writes until the deadline kills it. The run answers the timeout outcome
\ with the capture grown past a page, and the capture's mapping is gone once the
\ scope ends.
variable PCMDT-YES-ADDR

: PCMDT-RUNAWAY ( CMD:command -- CMD:command )
   s" /usr/bin/yes" PCMDT-SHORT-TIMEOUT-MS >MS CMD:RUN-OUTCOME T-OUTCOME-TIMEOUT
   CMD:OUT$ {: a:ptr u:n :}
   u PCMDT-PAGE-PROBE > TTRUE
   a NULL-PTR BYTE-VIEW - PCMDT-YES-ADDR !
   PCMDT-YES-ADDR @ PCMDT-MAPPED 0 T= ;

: PCMDT-RUNAWAY-FREED ( -- )
   [: PCMDT-RUNAWAY ;] CMD:WITH-COMMAND
   PCMDT-YES-ADDR @ PCMDT-MAPPED -1 T= ;

\ ---- a swapped token is refused ------------------------------------------------
\ The inner body swaps and hands back the outer token. The inner scope refuses it
\ with CMD:E-HANDLE and closes its own context; the throw leaves the outer scope,
\ which closes its own, before the outer body can run on a closed context. Both
\ scopes' capture mappings are gone after the refusal, and a fresh scope then runs
\ as usual.
variable PCMDT-OUTER-ADDR
variable PCMDT-INNER-ADDR

: PCMDT-FAT-ADDR ( CMD:command -- CMD:command n )
   PCMDT-FAT-RUN
   CMD:OUT$ drop NULL-PTR BYTE-VIEW -
   dup PCMDT-MAPPED 0 T= ;

: PCMDT-SWAP-INNER ( CMD:command CMD:command -- CMD:command CMD:command )
   PCMDT-FAT-ADDR PCMDT-INNER-ADDR ! swap ;

: PCMDT-SWAP-OUTER ( CMD:command -- CMD:command )
   PCMDT-FAT-ADDR PCMDT-OUTER-ADDR !
   [: PCMDT-SWAP-INNER ;] CMD:WITH-COMMAND
   s" unreached" PCMDT-SAY ;

: PCMDT-FRESH ( CMD:command -- CMD:command )
   s" fresh" PCMDT-SAY
   CMD:OUT$ s" fresh" T$= ;

: PCMDT-SWAPPED ( -- )
   [: [: PCMDT-SWAP-OUTER ;] CMD:WITH-COMMAND ;] CMD:E-HANDLE TTHROWSQ
   PCMDT-OUTER-ADDR @ PCMDT-MAPPED -1 T=
   PCMDT-INNER-ADDR @ PCMDT-MAPPED -1 T=
   [: PCMDT-FRESH ;] CMD:WITH-COMMAND ;

: PROCESS-COMMAND-TEST-MAIN ( -- )
   T-RESET
   PCMDT-FREED-ON-THROW
   PCMDT-SWAPPED
   [: PCMDT-PRINTF ;] CMD:WITH-COMMAND
   [: PCMDT-STDIN ;] CMD:WITH-COMMAND
   [: PCMDT-HERMETIC-ENV ;] CMD:WITH-COMMAND
   [: PCMDT-ENTRY-ENV ;] CMD:WITH-COMMAND
   [: PCMDT-INHERITED-ENV ;] CMD:WITH-COMMAND
   [: PCMDT-RC-ARMS ;] CMD:WITH-COMMAND
   [: PCMDT-TIMEOUT ;] CMD:WITH-COMMAND
   [: [: PCMDT-TIMEOUT-RC@ ;] CMD:WITH-COMMAND ;] E-PROC-TIMEOUT TTHROWSQ
   [: [: PCMDT-TIMEOUT-RUN-RC ;] CMD:WITH-COMMAND ;] E-PROC-TIMEOUT TTHROWSQ
   [: PCMDT-SPAWN-FAILS ;] CMD:WITH-COMMAND
   [: PCMDT-REFUSED-KEEPS ;] CMD:WITH-COMMAND
   PCMDT-CWD
   [: [: PCMDT-CWD-MISSING ;] CMD:WITH-COMMAND ;] E-PROC-PATH TTHROWSQ
   [: [: PCMDT-BAD-ENV-NAME ;] CMD:WITH-COMMAND ;] E-PROC-ENV TTHROWSQ
   [: [: PCMDT-BAD-ENV-ENTRY ;] CMD:WITH-COMMAND ;] E-PROC-ENV TTHROWSQ
   [: PCMDT-BOTH-STREAMS ;] CMD:WITH-COMMAND
   PCMDT-IN-FILL
   [: PCMDT-BIG-STDIN ;] CMD:WITH-COMMAND
   [: PCMDT-MANY-ARGS ;] CMD:WITH-COMMAND
   [: PCMDT-MANY-ROWS ;] CMD:WITH-COMMAND
   [: PCMDT-NESTED ;] CMD:WITH-COMMAND
   PCMDT-TWO-TASKS
   PCMDT-HALTED-TASK
   PCMDT-RUNAWAY-FREED
   T-REPORT
   s" process-command-test: ok" type cr ;

PROCESS-COMMAND-TEST-MAIN

\ genio-test.f - GENIO:READ-LINE answers a device's lines of any length, an
\ empty one included.
\ The device's accept reads an in-process source that holds every line before
\ the first read, so no read waits; its end is the end of input.
\ A device write that faults is caught with the output state its catch saved,
\ so the next write reaches its device rather than fd 1.
\ WITH-IO puts its caller back on the terminal when the body closed the
\ caller's device and a device it opened took that row.
\ The REPL reads the current input device's lines, an empty one and one a word
\ reads included, and ends at its end of input.
\ Run: bin/hb --load src/lib/genio-test.f </dev/null
require src/lib/test.f
require src/lib/string.f
require src/lib/task.f
require src/lib/genio-core.f
require src/lib/process.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f
require src/lib/test/ending.f

package GENIO-TEST
private

$A constant LF
$61 constant CH-A
$62 constant CH-B
$63 constant CH-C
$64 constant CH-D
1000 constant LONG-LEN                 \ several READ-LINE reads, the last partial
512 constant EVEN-LEN                  \ READ-LINE's reads end on the line's end
700 constant TASK-LEN                  \ read by the worker
256 constant TAIL-LEN                  \ a last line that fills its chunk
$1000 constant SRC-CAP
$10 constant SINK-CAP

create SRC SRC-CAP allot
create SINK SINK-CAP allot

variable SRC-LEN
variable SRC-AT
variable DEV
variable GOT-LEN
variable GOT-SAME
variable GOT-NULL
variable WANT-CH
variable GOT-CODE
variable SINK-N
variable SINK-DEV
variable FAULTY-DEV
variable CALLER-DEV
variable REUSE-DEV
TASK:MIN-STACK TASK:TASK READER

\ ---- the source --------------------------------------------------------------

: PUT-BYTES ( n n -- ) {: len:n c:n :}
   len 0 ?do c SRC SRC-LEN @ + i + c! loop
   SRC-LEN @ len + SRC-LEN ! ;

: PUT-LINE ( n n -- )
   PUT-BYTES
   LF SRC SRC-LEN @ + c!
   SRC-LEN @ 1+ SRC-LEN ! ;

: PUT-ALL ( -- )
   0 SRC-LEN !  0 SRC-AT !
   LONG-LEN CH-A PUT-LINE
   EVEN-LEN CH-B PUT-LINE
   TASK-LEN CH-C PUT-LINE
   0 CH-A PUT-LINE
   TAIL-LEN CH-D PUT-BYTES ;

\ A line, without its newline, or its first `cap` bytes; the rest of a longer
\ line stays in the source for the next accept. -1 once the source is spent.
: SRC-ACCEPT ( ^u8 n -- n ) {: bytes:^ cap:n :}
   SRC-AT @ SRC-LEN @ >= if -1 exit then
   0
   begin ( len )
      dup cap >= if exit then
      SRC-AT @ SRC-LEN @ >= if exit then
      SRC-AT @ 1+ SRC-AT !
      SRC SRC-AT @ 1- + c@ LF = if exit then
      SRC SRC-AT @ 1- + c@ over bytes swap + c!
      1+
   again ;

: NO-EMIT ( n -- ) drop ;
: NO-KEY ( -- n ) GENIO:E-IO throw ;
: NO-READY? ( -- bool ) false ;
: NO-READ ( ^u8 n -- n ) {: a:^ cap:n :} 0 ;
: NO-ACCEPT ( ^u8 n -- n ) {: a:^ cap:n :} -1 ;
: NO-WRITE ( str -- ) {: s:str :} ;
: NOTHING ( -- ) ;

: OPEN-SRC ( -- )
   [: NO-EMIT ;] [: NO-KEY ;] [: NO-READY? ;] [: NO-READ ;]
   [: NO-WRITE ;] [: SRC-ACCEPT ;] [: NOTHING ;] [: NOTHING ;]
   0 GENIO:DEVICE GENIO:DEVICE>N DEV ! ;

\ ---- reading through the device ------------------------------------------------

: SAME? ( str n -- bool ) {: s:str c:n :}
   s STR:LEN 0 ?do s STR:DATA i + c@ c <> if false unloop exit then loop
   true ;

: DEV@ ( -- GENIO:device )
   DEV @ GENIO:>DEVICE ;


: LINE-BODY ( -- )
   GENIO:READ-LINE {: s:str :}
   s STR:DATA 0= GOT-NULL !  s STR:LEN GOT-LEN !  s WANT-CH @ SAME? GOT-SAME ! ;

: LINE-ROW ( -- )
   ['] LINE-BODY catch GOT-CODE ! ;

: ROW ( [ -- ] n -- ) {: body c:n :}
   -1 GOT-LEN !  false GOT-SAME !  false GOT-NULL !  0 GOT-CODE !  c WANT-CH !
   DEV@ GENIO:TERMINAL body GENIO:WITH-IO ;

: T-LINE ( n n str -- ) {: want:n c:n ls:str :}
   ls T-LABEL
   [: LINE-ROW ;] c ROW
   GOT-CODE @ 0 T=
   GOT-LEN @ want T=
   GOT-SAME @ TTRUE
   GOT-NULL @ TFALSE ;

: T-NULL-LINE ( str -- ) {: ls:str :}
   ls T-LABEL
   [: LINE-ROW ;] 0 ROW
   GOT-CODE @ 0 T=
   GOT-NULL @ TTRUE ;

\ ---- a device write that faults ---------------------------------------------
\ The sink appends what it is written; the faulty device's write reads address 0.

: SINK-WRITE ( str -- ) {: s:str :}
   s STR:LEN 0 ?do
      SINK-N @ SINK-CAP < if s STR:DATA i + c@ SINK SINK-N @ + c!  SINK-N @ 1+ SINK-N ! then
   loop ;

: FAULTY-WRITE ( str -- ) {: s:str :}
   NULL-PTR BYTE-VIEW c@ drop ;

: OUT-DEVICE ( [ str -- ] -- GENIO:device ) {: qwrite :}
   [: NO-EMIT ;] [: NO-KEY ;] [: NO-READY? ;] [: NO-READ ;]
   qwrite [: NO-ACCEPT ;] [: NOTHING ;] [: NOTHING ;] 0 GENIO:DEVICE ;

: FAULT-TYPE ( -- ) s" x" type ;
: SINK-TYPE ( -- ) s" y" type ;

: T-FAULTING-WRITE ( -- )
   s" a write after a caught faulting device write reaches its device" T-LABEL
   [: SINK-WRITE ;] OUT-DEVICE GENIO:DEVICE>N SINK-DEV !
   [: FAULTY-WRITE ;] OUT-DEVICE GENIO:DEVICE>N FAULTY-DEV !
   0 SINK-N !
   [: GENIO:TERMINAL FAULTY-DEV @ GENIO:>DEVICE [: FAULT-TYPE ;] GENIO:WITH-IO ;]
   catch GOT-CODE !
   GENIO:SELF GENIO:DEVICE>N {: self:n :}
   GENIO:TERMINAL SINK-DEV @ GENIO:>DEVICE [: SINK-TYPE ;] GENIO:WITH-IO
   FAULTY-DEV @ GENIO:>DEVICE GENIO:CLOSE
   SINK-DEV @ GENIO:>DEVICE GENIO:CLOSE
   GOT-CODE @ 110 T=
   self GENIO:TERMINAL GENIO:DEVICE>N T=
   SINK SINK-N @ >str s" y" T$= ;

: REUSE-BODY ( -- )
   CALLER-DEV @ GENIO:>DEVICE GENIO:CLOSE
   [: NO-WRITE ;] OUT-DEVICE GENIO:DEVICE>N REUSE-DEV ! ;

: T-RESTORE-REUSED ( -- )
   s" WITH-IO leaves its caller on the terminal when the body closed the caller's device" T-LABEL
   [: NO-WRITE ;] OUT-DEVICE dup GENIO:DEVICE>N CALLER-DEV !
   GENIO:OUTPUT!
   [: GENIO:TERMINAL GENIO:TERMINAL [: REUSE-BODY ;] GENIO:WITH-IO ;]
   catch GOT-CODE !
   GENIO:OUTPUT@ GENIO:DEVICE>N {: out:n :}
   GOT-CODE @ 0 T=
   out GENIO:TERMINAL GENIO:DEVICE>N T=
   REUSE-DEV @ GENIO:>DEVICE GENIO:CLOSE ;

\ ---- the worker ---------------------------------------------------------------
\ The worker's line is released on its exit chain, which TASK:JOIN has run.
: READER-BODY ( -- )
   DEV@ GENIO:INPUT!
   GENIO:READ-LINE {: s:str :}
   s CH-C SAME? if s STR:LEN else -1 then TASK:RETURN ;

: T-WORKER ( -- )
   s" a worker reads a 700-byte line" T-LABEL
   ['] READER-BODY READER TASK:ACTIVATE
   READER TASK:JOIN MATCH result
      ok OF TASK-LEN T= ENDOF
      err OF 0 T= ENDOF
   ;MATCH ;

: CLOSE-ALL ( -- )
   DEV@ GENIO:CLOSE ;

\ ---- the REPL over a device -----------------------------------------------------
\ Only a tty runs the REPL, so a child on a pty includes the fixture. It exits,
\ prompts on the tty again, or says ok to lines the device never sent.
0 constant REPL-HUNG-UP
1 constant REPL-AT-TTY
2 constant REPL-LOOPED

: REPL-ENDING ( -- n )
   begin
      s\"  ok\r\n\r\x1B[Khabu> " PTY-HARNESS:IN-BUF? if REPL-AT-TTY exit then
      s" DEVICE-AFTER" s\"  ok\r\n ok\r\n" PTY-HARNESS:AFTER? if REPL-LOOPED exit then
      PTY-HARNESS:MASTER-FD PTY-HARNESS:READ-STEP 0 < if REPL-HUNG-UP exit then
   again ;

\ A child left reading the tty gets its ^D; one that never ends is killed.
: REPL-STOP ( n -- ) {: how:n :}
   how REPL-AT-TTY = if 4 PTY-HARNESS:SEND-BYTE then
   how REPL-LOOPED = if PTY-HARNESS:CHILD-PID PROCESS:SIGKILL PROCESS:KILL-RAW drop then ;

: T-REPL-DEVICE ( -- )
   s" the REPL reads the input device's lines and ends at its end of input" T-LABEL
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:SPAWN-ON-PTY
   s" habu> " PTY-HARNESS:WAIT-FOR dup TTRUE if
      PTY-HARNESS:BUF-CLEAR
      s" include test/fixtures/genio-repl-device.f" PTY-HARNESS:SEND-LINE
      REPL-ENDING {: how:n :}
      how REPL-STOP
      how REPL-HUNG-UP T=
      s\" 300\r\nDEVICE-REPL" PTY-HARNESS:IN-BUF? TTRUE
      s" DEVICE-AFTER" PTY-HARNESS:IN-BUF? TTRUE
   then
   PTY-HARNESS:REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:BUF$ s" " oc 0 T-ENDING-EXITED=
   PTY-HARNESS:CLOSE-MASTER ;

public

: GENIO-TEST-RUN ( -- )
   T-RESET
   PUT-ALL
   OPEN-SRC
   LONG-LEN CH-A s" READ-LINE answers a 1000-byte line whole" T-LINE
   EVEN-LEN CH-B s" READ-LINE answers a 512-byte line whole" T-LINE
   T-WORKER
   0 CH-A s" READ-LINE answers an empty line, not the end of input" T-LINE
   TAIL-LEN CH-D s" READ-LINE answers a last line that fills its chunk and has no newline" T-LINE
   s" the end of input answers a null line" T-NULL-LINE
   CLOSE-ALL
   T-FAULTING-WRITE
   T-RESTORE-REUSED
   T-REPL-DEVICE
   T-REPORT ;

;package

GENIO-TEST:GENIO-TEST-RUN

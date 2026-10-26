\ genio.f - generic text I/O devices with a TCP connection device and the
\ routed REPL reader. The device table, the terminal and the dispatchers are
\ src/lib/genio-core.f; this file reopens its package GENIO and adds what needs
\ a transport or changes the process on load.
\
\ STORAGE CLASS. TASK-LOCAL: READ-LINE's line and the chunk a device's accept
\ fills are TASK:+USER rows, so two tasks may each be reading a line from their
\ own device.
\
\ LINES. ACCEPT on a stream is standard Forth's: it fills at most the caller's
\ buffer and leaves the rest of a longer line in the stream for the next ACCEPT.
\ READ-LINE has no length of its own: its per-task line grows to the longest
\ line its task has read, and is released when the task exits.
\
\ ROUTING. Requiring this file installs READ-LINE as REPL-READ, so the engine's
\ REPL reads from the current input device.
require src/lib/prelude.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/image-lifecycle.f
require src/lib/byte-buffer.f
require src/lib/net/tcp4.f
require src/lib/genio-core.f

package GENIO

private

$A constant LF

\ The line READ-LINE hands back, per task: two tasks may each be reading a
\ line from their own device. The line is a BUF (src/lib/byte-buffer.f) that
\ grows to the longest line its task has read: a device's accept fills the
\ chunk row and READ-LINE appends each chunk. LINE-OPEN marks the line live
\ once its release is registered: the task's exit chain for a task, image
\ preparation for the main task, which has no exit chain and whose row a
\ capture would otherwise carry.
$100 constant CHUNK-CAP
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and CHUNK-CAP TASK:+USER IO-CHUNK drop
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and BUF:HDR-BYTES TASK:+USER IO-LINE drop
TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and $8 TASK:+USER IO-LINE-LIVE drop

: CHUNK ( -- ptr u8 )
   IO-CHUNK BYTE-VIEW ;

: LINE-DISPOSE ( -- )
   0 IO-LINE-LIVE !
   IO-LINE BUF:DISPOSE ;

: LINE-REGISTER ( -- )
   TASK:SELF-N 0= if [: LINE-DISPOSE ;] IMAGE-LIFECYCLE:REGISTER exit then
   [: LINE-DISPOSE ;] TASK:SELF TASK:AT-EXIT ;

\ A refused registration releases the line it would have released, so a live
\ line always has its release armed.
: LINE-OPEN ( -- )
   IO-LINE-LIVE @ 0<> if exit then
   IO-LINE CHUNK-CAP BUF:N>BLEN BUF:INIT
   [: LINE-REGISTER ;] catch {: code:n :}
   code 0<> if IO-LINE BUF:DISPOSE code throw then
   1 IO-LINE-LIVE ! ;

CAST: BLEN>N ( NUM:byte-len -- n )

\ The line is whole when an accept leaves the chunk unfilled: the newline, or
\ the end of input, came first.
: LINE-FILL ( -- )
   begin
      CHUNK CHUNK-CAP ACCEPT {: got:n :}
      CHUNK got BUF:N>BLEN IO-LINE BUF:APPEND-SPAN
      got CHUNK-CAP <
   until ;

public

\ The REPL's line reader, routed. src/habu/repl.f's RD-LINE is the terminal
\ device's accept and stays exactly that -- the tty editor with its history and
\ cursor keys -- while any other current input device answers with its own
\ accept. Requiring this file installs it as REPL-READ, so the engine's own REPL
\ loop and every other caller of that vector read from the current device with
\ no change to repl.f: that is what makes a REPL over a connection free.
: READ-LINE ( -- ptr u8 n )
   IN-ROW TERMINAL-ROW = if RD-LINE exit then
   LINE-OPEN
   IO-LINE BUF:CLEAR
   LINE-FILL
   IO-LINE BUF:SPAN$ BLEN>N ;

private

\ ---- the TCP connection device ----------------------------------------------
\ A stream carries bytes and has no terminal in it: emit and write send, key,
\ read and accept receive, key? is the non-blocking question TCP4 already
\ answers, and flush has nothing to do because a write is a send.

: SELF-CONNECTION ( -- TCP4:connection )
   SELF-STATE TCP4:>CONNECTION ;

: TCP-SEND ( ptr u8 n -- ) {: bytes:ptr len:n :}
   len 0 = if exit then
   SELF-CONNECTION bytes len TCP4:TRANSFER-BYTES TCP4:WRITE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF TCP4:ERRNO>N FAULT! ENDOF
   ;MATCH ;

: TCP-EMIT ( n -- ) {: c:n :}
   c SCRATCH c!
   SCRATCH 1 TCP-SEND ;

\ One transfer. `closed` and `failed` are both E-GENIO-IO here: neither can
\ answer with a byte, and answering zero for ever is the spin this must not do.
: TCP-RECEIVE ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   SELF-CONNECTION bytes cap TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF BLEN>N ENDOF
      closed OF drop E-IO throw ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

: TCP-KEY ( -- n )
   SCRATCH 1 TCP-RECEIVE drop
   SCRATCH c@ ;

\ TCP4's readiness question runs on the AIO loop (src/lib/aio.f), so a program
\ that asks a TCP device whether a byte is waiting starts the loop first; the
\ other operations of this device are transfers and need no loop.
: TCP-READY? ( -- bool )
   SELF-CONNECTION TCP4:READABLE?
   MATCH TCP4:ready-result
      ready OF true ENDOF
      idle OF false ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

\ One byte, answering false at the end of the stream instead of throwing: a
\ line reader has to be able to report the last partial line.
: TCP-BYTE? ( -- bool )
   SELF-CONNECTION SCRATCH 1 TCP4:TRANSFER-BYTES TCP4:READ
   MATCH TCP4:read-result
      data OF drop true ENDOF
      closed OF drop false ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

\ A line, without its newline, or its first `cap` bytes. Reading one byte at a
\ time is what keeps the stream's own bytes with the stream: a device has no
\ buffer of its own to leave a half-read line in for the next reader, and a
\ filled buffer stops before the next byte is taken, so the rest of a long line
\ is still the stream's. The running length stays on the data stack because a
\ quotation-free loop cannot rebind a local.
: TCP-ACCEPT ( ptr u8 n -- n ) {: bytes:ptr cap:n :}
   0
   begin ( len )
      dup cap >= if exit then
      TCP-BYTE? 0= if exit then
      SCRATCH c@ LF = if exit then
      dup bytes swap +  SCRATCH c@ swap c!
      1+
   again ;

: TCP-CLOSE ( -- )
   SELF-CONNECTION TCP4:CLOSE
   MATCH TCP4:status
      ok OF ENDOF
      failed OF drop E-IO throw ENDOF
   ;MATCH ;

: TCP-NOTHING ( -- ) ;

public

\ A connected TCP4 stream as a device. The caller still owns the connection's
\ lifetime: GENIO:CLOSE on this device is TCP4:CLOSE on that connection.
: TCP-DEVICE ( TCP4:connection -- device )
   TCP4:CONNECTION>N {: fd:n :}
   [: TCP-EMIT ;] [: TCP-KEY ;] [: TCP-READY? ;] [: TCP-RECEIVE ;]
   [: TCP-SEND ;] [: TCP-ACCEPT ;] [: TCP-NOTHING ;] [: TCP-CLOSE ;]
   fd DEVICE ;

private

: INSTALL-REPL-READ ( -- )
   [: READ-LINE ;] is REPL-READ ;

INSTALL-REPL-READ

;package

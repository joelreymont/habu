\ A definition held open across tty lines. The line reader is compiled code
\ that runs between the lines, and the open definition holds the compile
\ loop's bands RW; its end of input is a rejection, rc 70, since no input
\ remains for the REPL to recover into.
require src/lib/test/assert.f
require src/lib/test/ending.f
require src/lib/pty-harness.f
require src/lib/engine-candidate.f

package REPL-OPEN-DEF-TEST

\ Only the prompt after an answer shows the child reading raw again, so a step
\ waits for that one and ^D goes only after a step that saw it.
: STEP ( str -- bool )
   PTY-HARNESS:BUF-CLEAR PTY-HARNESS:SEND-LINE PTY-HARNESS:WAIT-ANSWER ;

: OPEN ( -- bool )
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:SPAWN-ON-PTY
   s" habu> " PTY-HARNESS:WAIT-FOR ;

\ Reap the child and assert it exited with code want. The terminal carries both
\ streams, and its buffer holds what the reap drained; a signal fails the case.
: EXIT= ( n -- )
   {: want:n :}
   PTY-HARNESS:REAP {: oc :}
   ENGINE-CANDIDATE:PATH$ PTY-HARNESS:BUF$ s" " oc want T-ENDING-EXITED=
   PTY-HARNESS:CLOSE-MASTER ;

\ The body of the definition SPAN-CASE opened, on its own line, defines a
\ word that runs.
: SPAN-LINES ( -- )
   s" 4321 ;" STEP TTRUE
   s" SPAN-X ." STEP TTRUE
   s" 4321" PTY-HARNESS:IN-BUF? TTRUE ;

\ A branch opened on one line resolves on the next.
: BRANCH-LINES ( -- )
   s" : SPAN-W ( n -- n )" STEP TTRUE
   s" dup 0 > if 100 +" STEP TTRUE
   s" else 100 - then ;" STEP TTRUE
   s" 7 SPAN-W ." STEP TTRUE
   s" 107" PTY-HARNESS:IN-BUF? TTRUE
   s" -7 SPAN-W ." STEP TTRUE
   s" -107" PTY-HARNESS:IN-BUF? TTRUE ;

: SPAN-CASE ( -- )
   OPEN dup TTRUE if
      s" : SPAN-X ( -- n )" STEP dup TTRUE if
         SPAN-LINES
         BRANCH-LINES
         4 PTY-HARNESS:SEND-BYTE
      then
   then
   0 EXIT= ;

\ A body line: 1400 of them pass the first chunk the REPL keeps its lines in.
: FILL-LINE$ ( -- str )
   s" 1 drop 1 drop 1 drop 1 drop 1 drop 1 drop" ;

\ ^D with the definition still open, its name on a line an older chunk keeps,
\ underlines the whole name and exits 70.
: EOF-CASE ( -- )
   OPEN dup TTRUE if
      s" : EOF-Y ( -- )" STEP dup TTRUE if
         true 1400 0 do FILL-LINE$ STEP and loop TTRUE
         4 PTY-HARNESS:SEND-BYTE
      then
   then
   70 EXIT=
   s" error[E-VS-UNTERMINATED-DEFINITION]" s" `EOF-Y`" PTY-HARNESS:AFTER? TTRUE
   s" error[E-VS-UNTERMINATED-DEFINITION]" s" |   ^^^^^ never closed"
   PTY-HARNESS:AFTER? TTRUE ;

\ ^D with a declaration still open names it and exits 70.
: DECL-EOF-CASE ( -- )
   OPEN dup TTRUE if
      s" BEGIN-STRUCTURE EOF-REC" STEP dup TTRUE if
         4 PTY-HARNESS:SEND-BYTE
      then
   then
   70 EXIT=
   s" error[E-UNCLOSED]" s" `BEGIN-STRUCTURE`" PTY-HARNESS:AFTER? TTRUE
   s" error[E-UNCLOSED]" s" error[E-UNCLOSED]" PTY-HARNESS:AFTER? TFALSE ;

\ A definition opened and closed on later lines leaves the declaration's
\ record on the line that opened it.
: DECL-LINES-CASE ( -- )
   OPEN dup TTRUE if
      s" BEGIN-STRUCTURE EOF-REC2" STEP TTRUE
      s" : EOF-FOO ( -- )" STEP TTRUE
      s" 1 2 + drop ;" STEP dup TTRUE if
         4 PTY-HARNESS:SEND-BYTE
      then
   then
   70 EXIT=
   s" error[E-UNCLOSED]" s" `BEGIN-STRUCTURE`" PTY-HARNESS:AFTER? TTRUE
   s" error[E-UNCLOSED]" s" | BEGIN-STRUCTURE EOF-REC2" PTY-HARNESS:AFTER? TTRUE
   s" error[E-UNCLOSED]" s" error[E-UNCLOSED]" PTY-HARNESS:AFTER? TFALSE ;

\ A `FUNCTION:` left open is rejected, its opener spelled as typed and its
\ record placed on the line that opened it, when a later line opens another.
: FN-LINES-CASE ( -- )
   OPEN dup TTRUE if
      s" require src/lib/ffi-abi.f" STEP TTRUE
      s" PROCESS-SYMBOLS" STEP TTRUE
      s" FUNCTION: FNL-PID getpid ( -- n )" STEP TTRUE
      PTY-HARNESS:BUF-CLEAR s" 1 drop FUNCTION: FNL-PPID getppid ( -- n )" PTY-HARNESS:SEND-LINE
      s" E-UNCLOSED" PTY-HARNESS:WAIT-FOR TTRUE
      s" E-UNCLOSED" s" habu> " PTY-HARNESS:WAIT-AFTER TTRUE
      s" error[E-UNCLOSED]" s" `FUNCTION:` is still open" PTY-HARNESS:AFTER? TTRUE
      s" error[E-UNCLOSED]" s" | FUNCTION: FNL-PID getpid ( -- n )" PTY-HARNESS:AFTER? TTRUE
      s" error[E-UNCLOSED]" s" error[E-UNCLOSED]" PTY-HARNESS:AFTER? TFALSE
      4 PTY-HARNESS:SEND-BYTE
   then
   0 EXIT= ;

\ A definition still open inside an open declaration is reported, on its own
\ line, and the declaration is not.
: DEF-AFTER-DECL-CASE ( -- )
   OPEN dup TTRUE if
      s" BEGIN-STRUCTURE EOF-REC3" STEP TTRUE
      s" : EOF-BAR ( -- )" STEP dup TTRUE if
         4 PTY-HARNESS:SEND-BYTE
      then
   then
   70 EXIT=
   s" error[E-VS-UNTERMINATED-DEFINITION]" s" | : EOF-BAR ( -- )" PTY-HARNESS:AFTER? TTRUE
   s" error[E-UNCLOSED]" PTY-HARNESS:IN-BUF? TFALSE ;

\ A failed definition must unwind through the line savepoint, then accept a
\ new definition.
: REJECT-CASE ( -- )
   OPEN dup TTRUE if
      PTY-HARNESS:BUF-CLEAR s" : REJECT-X ( -- ) NO-SUCH-WORD ;" PTY-HARNESS:SEND-LINE
      s" E-UNDEFINED" PTY-HARNESS:WAIT-FOR TTRUE
      s" E-UNDEFINED" s" habu> " PTY-HARNESS:WAIT-AFTER
      {: resumed:bool :}
      resumed TTRUE
      resumed if
         s" : AFTER-REJECT ( -- n ) 55 ;" STEP TTRUE
         s" AFTER-REJECT ." STEP TTRUE
         s" 55" PTY-HARNESS:IN-BUF? TTRUE
         4 PTY-HARNESS:SEND-BYTE
      then
   then
   0 EXIT= ;

\ A line whose throw computes 0 still fails: it answers `?`, and a definition
\ on the next line works. The REPL reads every line into one buffer, so
\ `included` on the line after a path literal is placed at itself, not at that
\ literal.
: ZERO-THROW-CASE ( -- )
   OPEN dup TTRUE if
      s" 5 5 - throw" STEP TFALSE
      s\" ?\r\n\r\x1B[Khabu> " PTY-HARNESS:IN-BUF? TTRUE
      s" : AFTER-ZERO ( -- n ) 56 ;" STEP TTRUE
      s" AFTER-ZERO ." STEP TTRUE
      s" 56" PTY-HARNESS:IN-BUF? TTRUE
      s\" s\" nofile.f\"" STEP TTRUE
      PTY-HARNESS:BUF-CLEAR s" included   " PTY-HARNESS:SEND-LINE
      s" E-MISSING-SOURCE" PTY-HARNESS:WAIT-FOR TTRUE
      s" E-MISSING-SOURCE" s" habu> " PTY-HARNESS:WAIT-AFTER TTRUE
      s" --> <input>:1:1" PTY-HARNESS:IN-BUF? TTRUE
      4 PTY-HARNESS:SEND-BYTE
   then
   0 EXIT= ;

\ Once EOF ends the session, a throwing exit hook is an uncaught exit, not a
\ jump back into the reader's retired REPL savepoint: on both targets the
\ hook's throw ends the process with its code.
: HOOK-CASE ( -- )
   OPEN dup TTRUE if
      s" require src/lib/at-exit.f" STEP TTRUE
      S\" : RV-HOOK ( -- ) s\" HOOK\" type cr 91 throw ;" STEP TTRUE
      s" ' RV-HOOK AT-EXIT:ADD" STEP TTRUE
      4 PTY-HARNESS:SEND-BYTE
   then
   91 EXIT=
   s" HOOK" PTY-HARNESS:IN-BUF? TTRUE
   s" E-STACK-BOUNDS" PTY-HARNESS:IN-BUF? TFALSE ;

\ The same hook armed while `die` runs inside a `catch`. A tty REPL recovers an
\ otherwise uncaught throw at its prompt, and the program's catch would return
\ from `die`; `die` disarms both before the hook runs, so the hook's throw ends
\ the process with its code and no prompt follows die's message.
: DIE-HOOK-CASE ( -- )
   OPEN dup TTRUE if
      s" require src/lib/at-exit.f" STEP TTRUE
      s" : CHILD-HOOK ( -- ) 9 throw ;" STEP TTRUE
      s" ' CHILD-HOOK AT-EXIT:ADD" STEP TTRUE
      S\" : CHILD-DIE ( n -- ) 0< if s\q neg\q 3 die then ;" STEP TTRUE
      s" : CHILD-MAIN ( -- n ) [: -1 CHILD-DIE ;] catch ;" STEP TTRUE
      PTY-HARNESS:BUF-CLEAR s" CHILD-MAIN" PTY-HARNESS:SEND-LINE
   then
   9 EXIT=
   s" neg" PTY-HARNESS:IN-BUF? TTRUE
   s" neg" s" habu> " PTY-HARNESS:AFTER? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SPAN-CASE
   EOF-CASE
   DECL-EOF-CASE
   DECL-LINES-CASE
   FN-LINES-CASE
   DEF-AFTER-DECL-CASE
   REJECT-CASE
   ZERO-THROW-CASE
   HOOK-CASE
   DIE-HOOK-CASE
   T-REPORT ;

;package

REPL-OPEN-DEF-TEST:RUN

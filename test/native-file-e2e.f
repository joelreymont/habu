\ native-file-e2e.f - what the native FILE end-to-end rows share: sources
\ written under one TMPDIR root, a child run with its stdout, stderr and exit
\ code kept, and the pass assert that prints both streams on a nonzero code.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/subject.f
require test/native-file-app-lib.f

package NATIVE-FILE-E2E
private

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
DYNAMIC-BUFFER OUT u8  variable OUT-U
DYNAMIC-BUFFER ERR u8  variable ERR-U
create PROGRAM CAP allot  variable PROGRAM-U

\ A child's streams grow to what it writes.
: OUT-ROOM ( n -- ptr u8 ) OUT-RESERVE 0 OUT ;
: ERR-ROOM ( n -- ptr u8 ) ERR-RESERVE 0 ERR ;

public

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
\ An empty stream reserved nothing, so it has no byte to view.
: OUT$ ( -- ptr u8 n ) OUT-U @ 0= if s" " exit then 0 OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR-U @ 0= if s" " exit then 0 ERR ERR-U @ ;

\ out and err become OUT$ and ERR$, as a capture leaves a child's streams.
: STREAMS! ( ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n err:ptr erru:n :}
   outu OUT-U ! outu 0<> if out outu OUT-ROOM outu BYTE-COPY then
   erru ERR-U ! erru 0<> if err erru ERR-ROOM erru BYTE-COPY then ;

\ The root named n under TMPDIR.
: TREE ( ptr u8 n -- )
   FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U ! ;

: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ exe with the staged argv and environment and the given stdin, under the hang
\ guard; the streams stay in OUT$ and ERR$. On 124, the capture's own deadline
\ or one the child met, SUBJECT:RC-TIMED-OUT prints the program and its streams
\ and throws E-PROC-TIMEOUT; any other code is the caller's to assert.
: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   0 PROGRAM-U ! exe exeu stdin stdinu PROGRAM CAP PROGRAM-U PROC-ARGV-PROGRAM+
   exe exeu >LEN stdin stdinu >LEN
   [: OUT-ROOM ;] [: ERR-ROOM ;] 600000 >MS
   RUN-ARGV-ENV-STDIN-GROWING-BOTH-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   PROGRAM PROGRAM-U @ OUT$ ERR$ rc SUBJECT:RC-TIMED-OUT
   rc ;

: RUN-APP ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" app-hb" AT s" " CAPTURE ;

\ tools/hb-build.f on the candidate engine: root/app.f into root/app-hb as a
\ REPL build with the ambient out root (tools/build-out.f). It runs at tier 1
\ (test/compiler/aot-mode.f): hb-build builds at its caller's tier, and only a
\ tier 1 product compiles through file objects. The tool library loads first,
\ at the default tier, so only the product compiles at tier 1.
\ HABU_FIXPOINT_ENGINE names the same engine, so the builder image hb-build spawns
\ (tools/app-build-image.f, BUILD-FIXPOINT:BF-ENGINE$) comes from it and
\ never from the bin/hb fallback.
: BUILD-APP ( -- n )
   NATIVE-FILE-APP:BUILDER$ 2drop
   PROC-ARGV-ENV-RESET
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build-lib.f" ARG s" test/compiler/aot-mode.f" ARG
   s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

: SUCCESS ( n -- )
   dup 0<> if OUT$ type ERR$ type then
   0 T= ;

;package

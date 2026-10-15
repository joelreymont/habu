\ native-file-e2e.f - what the native FILE end-to-end rows share: sources
\ written under one HB_TMP root, a child run with its stdout, stderr and exit
\ code kept, and the pass assert that prints both streams on a nonzero code.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package NATIVE-FILE-E2E
private

$8000 constant CAP
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot
create OUT CAP allot  variable OUT-U
create ERR CAP allot  variable ERR-U

public

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ out and err become OUT$ and ERR$, as a capture leaves a child's streams.
: STREAMS! ( ptr u8 n ptr u8 n -- )
   {: out:ptr outu:n err:ptr erru:n :}
   0 OUT-U ! out outu OUT CAP OUT-U BUF-APPEND
   0 ERR-U ! err erru ERR CAP ERR-U BUF-APPEND ;

\ The root named n under HB_TMP.
: TREE ( ptr u8 n -- )
   HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U ! ;

: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ exe with the staged argv and environment and the given stdin, under the hang
\ guard; the streams stay in OUT$ and ERR$, and 124 is an expired deadline.
: CAPTURE ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n stdin:ptr stdinu:n :}
   exe exeu >LEN stdin stdinu >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U !
   rc ;

: RUN-APP ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" app-hb" AT s" " CAPTURE ;

\ tools/hb-build.f on the candidate engine: root/app.f into root/app-hb as a
\ REPL build with the object cache at root/cache. HABU_FIXPOINT_ENGINE names
\ the same engine, so the builder image hb-build spawns
\ (tools/app-build-image.f, BUILD-FIXPOINT:BF-ENGINE$) comes from it and
\ never from the bin/hb fallback.
: BUILD-APP ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

: SUCCESS ( n -- )
   dup 0<> if OUT$ type ERR$ type then
   0 T= ;

;package

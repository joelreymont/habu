\ Build test/zlib-source.f once as a stripped application and run every ZLIB
\ case in that one process: libz resolves at the image's first foreign call.
\ The printed tree retains the executable, original.bin, compressed.bin and
\ decoded.bin; the application compares original and decoded bytes itself.
\ Run: bin/hb --load test/zlib-e2e.f
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/process.f
require test/pcap-result.f
require test/aot-builder-engine.f

package ZLIB-E2E
private

$8000 constant IO-CAP
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: FILE$ ( ptr u8 n -- ptr u8 n ) {: rel:ptr size:n :}
   ROOT$ rel size PATH FS:JOIN-PATH PATH swap ;
: APP$ ( -- ptr u8 n ) s" zlib-proof" FILE$ ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: RUN ( ptr u8 n -- ) {: program:ptr size:n :}
   PROC-ENV-INHERIT-MISSING
   program size >LEN SOURCE-ROOT:CWD$ >LEN s" " >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: BUILD ( -- )
   AOT-BUILDER-ENGINE:ENSURE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" test/zlib-source.f" ARG s" -o" ARG APP$ ARG
   ENGINE-CANDIDATE:PATH$ RUN
   s" the ZLIB cases build as a stripped application" T-LABEL
   NEED-OK APP$ FS:EXECUTABLE? TTRUE ;

: NATIVE-RUN ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" ARG ROOT$ ARG
   APP$ RUN
   s" the application passes every case and keeps its artifacts" T-LABEL
   NEED-OK
   OUT OUT-U @ s" zlib-source: ok" CONTAINS? TTRUE
   s" original.bin" FILE$ FS:FILE? TTRUE
   s" compressed.bin" FILE$ FS:FILE? TTRUE
   s" decoded.bin" FILE$ FS:FILE? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" zlib" FS:MKTEMP-DIR {: root:ptr size:n :}
   root ROOT size BYTE-COPY size ROOT-U !
   BUILD NATIVE-RUN
   s" zlib artifact: " type ROOT$ type cr
   T-REPORT ;
;package

ZLIB-E2E:MAIN

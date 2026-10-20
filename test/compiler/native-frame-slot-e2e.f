\ Build the source through production hb-build and run its retained executable.
require test/gate-common.f
require src/lib/engine-candidate.f
require test/preloaded-engine.f

package FRAME-SLOT-E2E
private
create SOURCE FS-PATH-CAP allot variable SOURCE-U
create IMAGE FS-PATH-CAP allot variable IMAGE-U
create OUTPUT FS-PATH-CAP allot variable OUTPUT-U

: SOURCE$ ( -- ptr u8 n ) SOURCE SOURCE-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: OUTPUT$ ( -- ptr u8 n ) OUTPUT OUTPUT-U @ ;

: PATHS ( -- )
   s" source.f" SOURCE GT-PATH SOURCE-U !
   s" frame-slots" IMAGE GT-PATH IMAGE-U !
   s" output.txt" OUTPUT GT-PATH OUTPUT-U ! ;

: BUILD ( -- )
   PRELOADED-ENGINE:LINKER$ {: linker:ptr linkeru:n :}
   s" test/compiler/native-frame-slot-image.f" SOURCE$ COPY-FILE-STREAM
   GE-HB-RESET
   s" --load" GE-ARG+
   s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+
   SOURCE$ GE-ARG+
   s" -o" GE-ARG+
   IMAGE$ GE-ARG+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN linker linkeru >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ 600000 GE-RUN-ENV
   s" frame slot production build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" frame slot executable" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   OUTPUT$ GT-OUT$ WRITE-ALL
   s" frame slot image run" GE-EXPECT-OK
   S\" frame-slots: ok\n" s" frame slot cases" GE-EXPECT-OUT ;

public
: RUN ( -- )
   GT-RESET
   s" native-frame-slots" HB-TMP-MKDIR GT-COPY-ROOT!
   PATHS BUILD RUN-IMAGE
   s" artifacts: " type GT-ROOT type cr
   s" PASS: native frame slots" type cr ;
;package

FRAME-SLOT-E2E:RUN

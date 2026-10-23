\ Build one subject through production hb-build on the keyed linker and run its
\ retained executable. The cases share that build: frame slots
\ (test/compiler/native-frame-slot-image.f), a tier-1 zero divide the stripped
\ image catches by its code (test/compiler/native-div-image.f) and decoded
\ literals across the old 4096-byte elaborator limit, generated here. MAIN runs
\ each package in turn and the output is compared byte for byte. The printed
\ directory keeps the source and image.
require test/gate-common.f
require src/lib/engine-candidate.f
require test/linker-builder-engine.f

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

: LITERAL ( n -- ) {: size:n :}
   s" s" GE-SRC+
   34 GE-SRC-C
   32 GE-SRC-C
   size 0 ?do i 26 mod 97 + GE-SRC-C loop
   34 GE-SRC-C ;

: DEFINITION ( ptr u8 n n -- ) {: name:ptr nameu:n size:n :}
   s" : " GE-SRC+ name nameu GE-SRC+
   s"  ( -- ptr u8 n ) " GE-SRC+
   size LITERAL
   s"  ;" GE-SRC-LINE ;

: LONG-LITERALS ( -- )
   s" package NLB-SUBJECT" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" : CHECK ( ptr u8 n n -- ) {: p:ptr u:n want:n :}" GE-SRC-LINE
   s"    u want <> if 71 throw then" GE-SRC-LINE
   s"    u 0 ?do p i + c@ i 26 mod 97 + <> if 72 throw then loop ;" GE-SRC-LINE
   s" LONG-4096" 4096 DEFINITION
   s" LONG-4097" 4097 DEFINITION
   s" LONG-4135" 4135 DEFINITION
   S\" : EMPTY ( -- ptr u8 n ) s\q \q ;" GE-SRC-LINE
   s" SMALL-LITERAL" 3 DEFINITION
   s" public" GE-SRC-LINE
   s" : RUN ( -- ) LONG-4096 4096 CHECK LONG-4097 4097 CHECK LONG-4135 4135 CHECK" GE-SRC-LINE
   s"    EMPTY nip 0 <> if 73 throw then" GE-SRC-LINE
   S\"    SMALL-LITERAL 3 CHECK s\q long: ok\q type cr ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE ;

: WRITE-SOURCE ( -- )
   GE-SRC-RESET
   s" test/compiler/native-frame-slot-image.f" GE-SRC-FILE+
   s" test/compiler/native-div-image.f" GE-SRC-FILE+
   LONG-LITERALS
   s" : MAIN ( -- ) FRAME-SLOT-IMAGE:CASES NDIV-IMAGE:RUN NLB-SUBJECT:RUN ;" GE-SRC-LINE
   SOURCE$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: BUILD ( -- )
   LINKER-BUILDER-ENGINE:ENSURE
   LINKER-BUILDER-ENGINE:HOST$ {: linker:ptr linkeru:n :}
   WRITE-SOURCE
   GE-HB-RESET
   s" --load" GE-ARG+
   s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+
   SOURCE$ GE-ARG+
   s" -o" GE-ARG+
   IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN linker linkeru >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ 600000 GE-RUN-ENV
   s" production build" GE-EXPECT-OK
   GT-ERR$ nip 0 <> if s" production build writes no diagnostic" GE-FAIL then
   IMAGE$ EXECUTABLE? 0= if s" production executable" GE-FAIL then ;

\ An uncaught -6400 would exit 0, a multiple of 256, and stop the output there.
: EXPECTED$ ( -- ptr u8 n )
   S\" PAIR\n20\n73\nMERGE\n8\n74\nCT\n12\nCT2\n-10\n10\nALT\n50\n50\n40\nCL\n14\n3\nJN\n27\n18\nNEST\n4\n4\n9\nEXIT-EARLY\n8\n3\n8\n2\nframe-slots: ok\n-6400\n-1\n0\n0\n1\n0\n0\n1\n-1\n-7\n0\n-2\n-1\nlong: ok\n" ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   OUTPUT$ GT-OUT$ WRITE-ALL
   s" image run" GE-EXPECT-OK
   EXPECTED$ s" frame slot, division and long literal cases" GE-EXPECT-OUT ;

public
: RUN ( -- )
   GT-RESET
   s" native-frame-slots" HB-TMP-MKDIR GT-COPY-ROOT!
   PATHS BUILD RUN-IMAGE
   s" artifacts: " type GT-ROOT type cr
   s" PASS: native frame slots, stripped division, long literals" type cr ;
;package

FRAME-SLOT-E2E:RUN

\ Build and run native images with decoded literals across the old 4096-byte
\ elaborator limit. The printed directory retains both source files and images
\ so the same build command can be repeated after this test exits. The subjects
\ require nothing, so each build's maker runs on the keyed linker image
\ (test/preloaded-engine.f) and compiles the literals as it loads the subject.
require test/gate-common.f
require lib/engine-candidate.f
require test/preloaded-engine.f

package NATIVE-LONG-STRING-IMAGE-TEST

create SUBJECT FS-PATH-CAP allot  variable SUBJECT-U
create IMAGE FS-PATH-CAP allot    variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PATHS ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n output:ptr outputu:n :}
   source sourceu SUBJECT GT-PATH SUBJECT-U !
   output outputu IMAGE GT-PATH IMAGE-U ! ;

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

: SOURCE-START ( -- )
   GE-SRC-RESET
   s" package NLB-SUBJECT" GE-SRC-LINE
   s" private" GE-SRC-LINE
   s" : CHECK ( ptr u8 n n -- ) {: p:ptr u:n want:n :}" GE-SRC-LINE
   s"    u want <> if 71 throw then" GE-SRC-LINE
   s"    u 0 ?do p i + c@ i 26 mod 97 + <> if 72 throw then loop ;" GE-SRC-LINE ;

: FIRST-SOURCE ( -- )
   SOURCE-START
   s" LONG" 4096 DEFINITION
   s" public" GE-SRC-LINE
   S\" : RUN ( -- ) LONG 4096 CHECK s\q boundary: ok\q type cr ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : MAIN ( -- ) NLB-SUBJECT:RUN ;" GE-SRC-LINE ;

: SECOND-SOURCE ( -- )
   SOURCE-START
   s" LONG-4097" 4097 DEFINITION
   s" LONG-4135" 4135 DEFINITION
   S\" : EMPTY ( -- ptr u8 n ) s\q \q ;" GE-SRC-LINE
   s" SMALL-LITERAL" 3 DEFINITION
   s" public" GE-SRC-LINE
   s" : RUN ( -- ) LONG-4097 4097 CHECK LONG-4135 4135 CHECK" GE-SRC-LINE
   s"    EMPTY nip 0 <> if 73 throw then" GE-SRC-LINE
   S\"    SMALL-LITERAL 3 CHECK s\q long: ok\q type cr ;" GE-SRC-LINE
   s" ;package" GE-SRC-LINE
   s" : MAIN ( -- ) NLB-SUBJECT:RUN ;" GE-SRC-LINE ;

: BUILD ( -- )
   PRELOADED-ENGINE:LINKER$ {: linker:ptr linkeru:n :}
   SUBJECT$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL
   GE-HB-RESET
   s" --load" GE-ARG+
   s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+
   SUBJECT$ GE-ARG+
   s" -o" GE-ARG+
   IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN linker linkeru >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ GE-TIMEOUT-MS GE-RUN-ENV
   s" long literal production build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" long literal executable" GE-FAIL then ;

: RUN-IMAGE ( ptr u8 n -- ) {: want:ptr wantu:n :}
   GE-HB-RESET
   IMAGE$ GE-TIMEOUT-MS GE-RUN-ENV
   s" long literal image exits cleanly" GE-EXPECT-OK
   want wantu s" long literal image verifies every byte" GE-EXPECT-OUT ;

: RUN ( -- )
   GT-RESET
   s" native-long-string" HB-TMP-MKDIR GT-COPY-ROOT!
   s" boundary.f" s" boundary" PATHS
   FIRST-SOURCE BUILD
   S\" boundary: ok\n" RUN-IMAGE
   s" long.f" s" long" PATHS
   SECOND-SOURCE BUILD
   S\" long: ok\n" RUN-IMAGE
   s" artifacts: " type GT-ROOT type cr
   s" PASS: long native literals build and run" type cr ;

RUN
;package

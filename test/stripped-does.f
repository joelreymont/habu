\ Baked defining words and a fresh does> clause link through their real image.
require test/gate-common.f
require lib/engine-candidate.f

package STRIPPED-DOES-TEST
private

600000 constant TIMEOUT-MS
create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   s" stripped-does" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U ! ;

: WRITE-SUBJECT ( -- )
   SB-RESET
   S\" require lib/aio.f\nBEGIN-STRUCTURE STRIP-REC-BYTES\nCELL +FIELD STRIP-FIELD\nEND-STRUCTURE\n0 ENUM+ STRIP-E0\n1 ENUM4+ STRIP-E4\n: STRIP-MAKE ( n -- ) create , does> ( -- n ) @ ;\n23 STRIP-MAKE STRIP-VALUE\n: MAIN ( -- )\n   STRIP-REC-BYTES . STRIP-E0 . STRIP-E4 . STRIP-VALUE .\n" SB-APPEND
   HB-TARGET-MACOS? if S\"    AIO:START AIO:STOP\n" SB-APPEND then
   S\" ;\n" SB-APPEND
   SUBJECT$ SB$ WRITE-ALL ;

: BUILD ( -- )
   GE-HB-RESET
   ENGINE-CANDIDATE:PATH$ GE-ARGV+
   s" --load" GE-ARG+ s" tools/hb-build.f" GE-ARG+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+
   s" -o" GE-ARG+ IMAGE$ GE-ARG+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_BUILD_CACHE" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS GE-RUN-ENV
   s" stripped does build" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? 0= if s" stripped does executable" GE-FAIL then ;

: RUN-IMAGE ( -- )
   GE-HB-RESET
   IMAGE$ GE-ARGV+
   IMAGE$ TIMEOUT-MS GE-RUN-ENV
   s" stripped does run" GE-EXPECT-OK
   GT-OUT$ S\" 8\n0\n1\n23\n" STR= 0= if
      s" stripped does exact stdout" GE-FAIL
   then
   GT-ERR$ nip 0<> if s" stripped does stderr" GE-FAIL then ;

: BODY ( -- )
   PREPARE WRITE-SUBJECT BUILD RUN-IMAGE
   s" PASS: stripped defining-word companions" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package

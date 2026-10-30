\ A public EXPORT and its private body share a name and code span in the
\ compiler bundle, but originate in different source files. Call the public
\ word from an application linked by the real stripped maker and check that
\ its emitted map carries the donor's source path for that span.
require test/gate-common.f
require test/compiler-images.f
require src/lib/process.f

package NATIVE-DONOR-MAP-E2E
private

create ENGINE PATH-CAP allot variable ENGINE-U
create SUBJECT PATH-CAP allot variable SUBJECT-U
create APP PATH-CAP allot variable APP-U
create MAP PATH-CAP allot variable MAP-U
DYNAMIC-BUFFER MAP-TEXT u8

: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;
: MAP$ ( -- ptr u8 n ) MAP MAP-U @ ;

: PREPARE ( -- )
   COMPILER-IMAGES:ROOT$ dup 0= if
      2drop
      s" native-donor-map: HABU_GATE_COMPILER_IMAGES is not set; initialize the suite compiler bundle before running this row" 75 die
   then
   s" hb-whitebox-source" ENGINE FS:JOIN-PATH ENGINE-U !
   s" native-donor-map" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" APP GT-PATH APP-U !
   s" hb-aot-got.map" MAP GT-PATH MAP-U !
   \ A call target resolves to its first dictionary record, which is the
   \ private body's record for this shared xt, so the map names its file.
   SUBJECT$
   S\" : MAIN ( -- ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-B drop ;\n" FS:WRITE-ALL ;

: BUILD ( -- )
   GE-HB-RESET
   ENGINE$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" --donor" GE-ARG+ ENGINE$ GE-ARG+
   s" TMPDIR" >LEN GT-ROOT >LEN PROC-ENV+
   ENGINE$
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
   PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN
   s" native donor stripped build" GE-EXPECT-OK ;

: CHECK-MAP ( -- )
   MAP$ FS:FILE-SIZE {: size:n :}
   size MAP-TEXT-RESERVE
   MAP$ 0 MAP-TEXT BYTE-VIEW size FS:READ-ALL size <> if
      s" native donor map short read" GE-FAIL then
   0 MAP-TEXT BYTE-VIEW size
      s" MAP-ORIGIN-B word baked test/compiler/native-map-b.f"
      CONTAINS? 0= if s" donor source path" GE-FAIL then ;

: RUN-APP ( -- )
   GE-HB-RESET
   APP$ GE-ARGV+
   APP$ PROCESS:WAIT-FOREVER MS>N GE-RUN-ENV
   s" native donor stripped image" GE-EXPECT-OK
   GT-OUT$ nip 0 <> if s" public word execution" GE-FAIL then ;

public
: RUN ( -- )
   PREPARE BUILD CHECK-MAP RUN-APP
   s" native donor map artifacts: " type GT-ROOT type cr ;

;package

NATIVE-DONOR-MAP-E2E:RUN

\ A public EXPORT and its private body share a name and code span in the
\ compiler bundle, but originate in different source files. Call the public
\ word from an application linked by the real stripped maker and check that
\ its emitted map carries the donor's source path for that span.
require test/gate-common.f
require test/c2-image-pair.f
require test/suite-budget.f

package NATIVE-DONOR-MAP-E2E
private

create ROOT-IMAGE FS-PATH-CAP allot variable ROOT-IMAGE-U
create SUBJECT FS-PATH-CAP allot variable SUBJECT-U
create APP FS-PATH-CAP allot variable APP-U
create MAP FS-PATH-CAP allot variable MAP-U
DYNAMIC-BUFFER MAP-TEXT u8

: ROOT-IMAGE$ ( -- ptr u8 n ) ROOT-IMAGE ROOT-IMAGE-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: APP$ ( -- ptr u8 n ) APP APP-U @ ;
: MAP$ ( -- ptr u8 n ) MAP MAP-U @ ;

: PREPARE ( -- )
   s" native-donor-map" GT-START
   GT-ROOT C2-IMAGE-PAIR:PROVIDE
   s" hb-root" ROOT-IMAGE GT-PATH ROOT-IMAGE-U !
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" hb-aot-got" APP GT-PATH APP-U !
   s" hb-aot-got.map" MAP GT-PATH MAP-U !
   \ A call target resolves to its first dictionary record, which is the
   \ private body's record for this shared xt, so the map names its file.
   SUBJECT$
   S\" : MAIN ( -- ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-B drop ;\n" WRITE-ALL ;

: BUILD ( -- )
   GE-HB-RESET
   ROOT-IMAGE$ GE-ARGV+
   s" --" GE-ARG+ SUBJECT$ GE-ARG+ s" 0" GE-ARG+
   s" --donor" GE-ARG+ ROOT-IMAGE$ GE-ARG+
   s" TMPDIR" >LEN GT-ROOT >LEN PROC-ENV+
   ROOT-IMAGE$
   S\" require tools/aot-build-open.f\nrequire tools/aot-build.f\nAOT-LINK:BUILD-NATIVE\n"
   SUITE-BUDGET:CHILD-MS GE-RUN-STDIN
   s" native donor stripped build" GE-EXPECT-OK ;

: CHECK-MAP ( -- )
   MAP$ FILE-SIZE {: size:n :}
   size MAP-TEXT-RESERVE
   MAP$ 0 MAP-TEXT BYTE-VIEW size READ-ALL size <> if
      s" native donor map short read" GE-FAIL then
   0 MAP-TEXT BYTE-VIEW size
      s" MAP-ORIGIN-B word baked test/compiler/native-map-b.f"
      CONTAINS? 0= if s" donor source path" GE-FAIL then ;

: RUN-APP ( -- )
   GE-HB-RESET
   APP$ GE-ARGV+
   APP$ SUITE-BUDGET:CHILD-MS GE-RUN-ENV
   s" native donor stripped image" GE-EXPECT-OK
   GT-OUT$ nip 0 <> if s" public word execution" GE-FAIL then ;

public
: RUN ( -- )
   PREPARE BUILD CHECK-MAP RUN-APP
   s" native donor map artifacts: " type GT-ROOT type cr ;

;package

NATIVE-DONOR-MAP-E2E:RUN

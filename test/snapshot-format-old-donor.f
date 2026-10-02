\ A version 12 native donor must refuse the current writer before it creates
\ an image. Run on the retained version 12 product while changing the format:
\   old-hb --load test/snapshot-format-old-donor.f
require test/gate-common.f
require lib/engine-id.f

package SNAPSHOT-FORMAT-OLD-DONOR-TEST
private

600000 constant TIMEOUT-MS
create IMAGE FS-PATH-CAP allot
variable IMAGE-U

: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   snapshot-format 12 <> if s" snapshot format test needs a version 12 donor" GE-FAIL then
   s" snapshot-format-old-donor" GT-START
   s" refused-image" IMAGE GT-PATH IMAGE-U ! ;

: REFUSED ( -- )
   GE-HB-RESET
   ENGINE-ID:PATH$ GE-ARGV+
   s" --" GE-ARG+ IMAGE$ GE-ARG+
   ENGINE-ID:PATH$
   S\" require src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   TIMEOUT-MS GE-RUN-STDIN
   74 s" old donor refuses the current snapshot writer" GE-EXPECT-RC
   GT-ERR$ s" snap: donor does not support snapshot format" CONTAINS? 0= if
      s" old donor refusal reason" GE-FAIL
   then
   IMAGE$ EXISTS? if s" old donor wrote an image" GE-FAIL then ;

: BODY ( -- )
   PREPARE REFUSED
   s" PASS: old donor refuses snapshot format" type cr ;

public
: RUN ( -- ) [: BODY ;] [: GT-CLEANUP ;] finally ;

RUN
;package

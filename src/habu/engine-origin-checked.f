\ Observe checked definitions between the hook installation and the full
\ engine-origin module's load. The prefix capture owner is already present.

package ENGINE-ORIGIN-EARLY

private

$388 constant DEF-TIER-OFF
$27B8 constant TRUSTED-OFF
: BODY-CELLS ( -- ptr u8 ) NATIVE-BODY:BODY-CAPABILITY ;

public

\ The optional record address is a runtime capability, not a baked host fact.
: BODY-CAPABILITY ( -- ptr u8 ) BODY-CELLS ;

\ Defer only the accepted ordinary native definition under this exact hook.
\ On an older kernel the immediate hook-time TAKE remains authoritative.
: NATIVE-ARM ( n n -- bool ) {: hook:n take:n :}
   data-base PEND-OFF + @ 0= if 0 0= 0= exit then
   data-base DEF-TIER-OFF + @ 1 <> if 0 0= 0= exit then
   data-base TRUSTED-OFF + @ 0 <> if 0 0= 0= exit then
   check@ hook <> if 0 0= 0= exit then
   BODY-CELLS dup NULL-PTR = if drop 0 0= 0= exit then
   {: record:ptr :}
   record NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW @ hook <>
   record NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW @ take <> or
   if 0 0= 0= exit then
   take record NATIVE-BODY-ABI:PENDING-TAKE cells + CELL-VIEW !
   0 0= ;

private

CAST: HOOK-N ( [ ptr u8 n -- n ] -- n )
CAST: TAKE-N ( [ ptr u8 n -- ] -- n )

: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1+ repeat
   a swap ;

: BODY-TAKE ( ptr u8 n -- ) FIRST-TOKEN CHECKED-TAKE ;

: CHECKED-HOOK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LOWER-CERT-HOOK:HOOK dup -1 = if
      a u FIRST-TOKEN {: name:ptr nameu:n :}
      check@ ['] BODY-TAKE TAKE-N NATIVE-ARM 0= if
         name nameu CHECKED-TAKE
      then
   then ;

public

: FILE-HOOK? ( -- bool ) check@ ['] CHECKED-HOOK HOOK-N = ;
TRUSTED: INSTALL ( -- )
   ['] CHECKED-HOOK set-check
   CAST-REG @ 0= if
      CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + CELL-VIEW @ CAST-REG xt!
   then
   ['] OBSERVE-CAST CAST-ENTRY
   CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + CELL-VIEW xt!
   ['] EXPORT-ACCEPT EXPORT-ENTRY
   BODY-CELLS dup NULL-PTR = if drop exit then
   {: record:ptr :}
   ['] CHECKED-HOOK HOOK-N
      record NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW !
   ['] BODY-TAKE TAKE-N
      record NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW ! ;

;package

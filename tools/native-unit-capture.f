\ Bind the retained compiler's producer words that object import calls.

require src/lib/errors.f
require src/compiler/native/publish.f
require src/compiler/session/emission.f
require src/compiler/native/string.f
require src/habu/xref.f
require src/habu/aot-arm.f
require src/lib/string.f
require tools/build-target.f

package NUNIT-CAPTURE

private

DYNAMIC-BUFFER PRODUCER-NAME ptr u8
DYNAMIC-BUFFER PRODUCER-LEN n
DYNAMIC-BUFFER PRODUCER-XT n
variable PRODUCER-N

CAST: WITH-UNIT-XT ( n -- [ [ n n n n -- ] [ -- ] -- ] )

: BIND-ONE ( ptr u8 n bool -- ) {: a:ptr u:n optional:bool :}
   a u XREF-FIND dup XREF-FOUND? if XREF-START
   else drop optional 0= if E-NUNIT-PROFILE throw then 0 then
      {: xt:n :}
   PRODUCER-N @ {: row:n :}
   row 1+ PRODUCER-NAME-RESERVE
   row 1+ PRODUCER-LEN-RESERVE
   row 1+ PRODUCER-XT-RESERVE
   a row PRODUCER-NAME !
   u row PRODUCER-LEN !
   xt row PRODUCER-XT !
   1 PRODUCER-N +! ;

: BOUND-XT ( ptr u8 n -- n ) {: a:ptr u:n :}
   PRODUCER-N @ 0= if E-NUNIT-PROFILE throw then
   PRODUCER-N @ 0 ?do
      i PRODUCER-NAME @ i PRODUCER-LEN @ a u STR= if
         i PRODUCER-XT @ unloop exit
      then
   loop
   E-NUNIT-PROFILE throw ;

: TARGET-XT ( ptr u8 n -- n )
   BOUND-XT dup 0= if E-NUNIT-PROFILE throw then ;

public

: BIND-PRODUCER ( -- )
   0 PRODUCER-N !
   s" NPUB:WITH-UNIT" false BIND-ONE
   s" NART:CALL-SITES" false BIND-ONE
   s" NART:CALL-SITE@" false BIND-ONE
   s" NART:CALL-KIND@" false BIND-ONE
   s" NART:CALL-TARGET@" false BIND-ONE
   s" NART:ADDR-SITES" false BIND-ONE
   s" NART:ADDR-SITE@" false BIND-ONE
   s" NART:ADDR-SITE-KIND@" false BIND-ONE
   s" NART:ADDR-SITE-ORIGIN@" true BIND-ONE
   s" NART:BYTES" false BIND-ONE
   s" NART:FUNCTION-OFFSET@" false BIND-ONE
   s" NART:CALL-IMPL@" false BIND-ONE
   s" NART:CALL-LOC@" false BIND-ONE
   s" native-unit-cut-cell" true BIND-ONE
   s" NHOST:TAKEN-FUNCTIONS" true BIND-ONE
   s" NHOST:TAKEN-FACTS" true BIND-ONE
   s" NHOST:TAKEN-TYPE@" true BIND-ONE
   s" NHOST:PREPARE-IMPORT" true BIND-ONE
   s" NHOST:ABANDON-TAKEN" true BIND-ONE
   s" NHOST:OBSERVE-RUNS" true BIND-ONE
   s" NHOST:UNOBSERVE-RUNS" true BIND-ONE
   s" CHECKER-OWNER:RESOLUTION-TOP" true BIND-ONE
   \ The checker unit codes a FILE object's checker mark and export answer.
   s" CHECKER-REG:E-UNIT-STATE" true BIND-ONE
   s" CHECKER-REG:E-UNIT-FORMAT" true BIND-ONE
   s" NPUB:IMPORT-PENDING" true BIND-ONE
   s" NCOMP:DEFINITION-CACHEABLE?" true BIND-ONE
   s" NCOMP:DEFINITION-HISTORY" true BIND-ONE
   s" NCOMP:REPLAY-DEFINITION" true BIND-ONE
   s" NSTR:PREPARE-BEGIN" true BIND-ONE
   s" NSTR:PREPARE-INTERN" true BIND-ONE
   s" NSTR:PREPARE-DATA-END" true BIND-ONE
   s" NSTR:PREPARE-CLOSE" true BIND-ONE
   s" NSTR:INTERN" true BIND-ONE
   s" NSTR:SOURCE-BEGIN" true BIND-ONE
   s" NSTR:SOURCE-END" true BIND-ONE
   s" NSTR:SOURCE-COUNT" true BIND-ONE
   s" NSTR:SOURCE@" true BIND-ONE
   s" REQUIRE-REG:COUNT" true BIND-ONE
   s" REQUIRE-REG:LEN" true BIND-ONE
   s" REQUIRE-REG:SLOT" true BIND-ONE
   s" TASK:OWNED-USER-NEXT" true BIND-ONE
   \ The storage owners a FILE object's DATA scan and updates skip.
   s" NSTR:CELL-SPANS" true BIND-ONE
   s" NSTR:OWNER-ROW" true BIND-ONE
   s" FFI:SCAN-SPANS" true BIND-ONE
   s" FFI-DECL:SCAN-SPANS" true BIND-ONE
   s" TASK:OWNED-CALLBACK-ROWS" true BIND-ONE
   s" TASK:OWNED-CELLS" true BIND-ONE
   s" DYNAMIC-STORAGE:OWNED-CELLS" true BIND-ONE
   s" REQUIRE-REG:CELL-SPANS" true BIND-ONE
   s" FFI-CB:SCAN-SPANS" true BIND-ONE
   s" FFI:FORGET-CACHE" true BIND-ONE
   \ A primitive's REACH class, which admits a top-level run's reach.
   s" PRIM-SPEC:REACH@" true BIND-ONE
   s" PRIM-SPEC:FIND" true BIND-ONE
   \ The checker's unit facts, one trace a top-level run may leave.
   s" CHECKER-OWNER:UNIT-FACTS" true BIND-ONE
   s" NSTR:SOURCE-CLOSE" true BIND-ONE
   \ The FFI registry's marks and rows a FILE object's segments carry.
   s" FFI:MARK" true BIND-ONE
   s" FFI:NEXT-FN" true BIND-ONE
   s" FFI:NEXT-LIB" true BIND-ONE
   s" FFI:FN-INDEX" true BIND-ONE
   s" FFI:LIB-INDEX" true BIND-ONE
   s" FFI:FN-ROW@" true BIND-ONE
   s" FFI:LIB-ROW@" true BIND-ONE
   s" FFI:MAX-ARGS" true BIND-ONE
   s" FFI:SYMBOL-CAP" true BIND-ONE
   s" FFI-DECL:SELECTION" true BIND-ONE
   s" FFI-DECL:SELECT" true BIND-ONE
   s" FFI:LIBRARY-PATH" true BIND-ONE
   s" FFI:DECLARE-SPILLED" true BIND-ONE
   s" FFI:OBSERVE" true BIND-ONE
   s" FFI:UNOBSERVE" true BIND-ONE
   s" FFI:MAKER-BANK" true BIND-ONE ;

: BOUND-PRODUCER-XT ( ptr u8 n -- n ) TARGET-XT ;
: OPTIONAL-PRODUCER-XT ( ptr u8 n -- n ) BOUND-XT ;

\ The run observer needs the kernel's hook and the checker owner's top-level
\ resolution legs (NUNIT-DEP OBSERVE-RUN); a kernel without either has none.
: RUN-OBSERVER? ( -- bool )
   s" NHOST:OBSERVE-RUNS" BOUND-XT 0<>
   s" CHECKER-OWNER:RESOLUTION-TOP" BOUND-XT 0<> and ;

\ A unit object is written under NUNIT-FILE:ARCH-AARCH64 and carries the code
\ and address sites of an AArch64 compiler. Check both the retained engine and
\ a selected body target: another profile caches no definition.
: AARCH64-PROFILE? ( -- bool )
   RTARGET:HOST-TARGET RTARGET:ARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   BUILD-TARGET:CURRENT RTARGET:ARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   and ;

\ The refusal behind AARCH64-PROFILE?. DEF-ELIGIBLE? answers it quietly so a
\ build for another target simply caches no definition; capturing, packing or
\ installing a unit body under another profile is a caller error and throws.
: PROFILE-CK ( -- )
   AARCH64-PROFILE? 0= if E-NUNIT-PROFILE throw then ;

: WITH-OBSERVER ( [ n n n n -- ] [ -- ] -- )
   s" NPUB:WITH-UNIT" TARGET-XT WITH-UNIT-XT execute ;

: CLOSE ( -- )
   PRODUCER-NAME-RELEASE PRODUCER-LEN-RELEASE PRODUCER-XT-RELEASE
   0 PRODUCER-N ! ;

;package

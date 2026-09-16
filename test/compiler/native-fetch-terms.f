\ Publishing a checked signature keeps the type terms of its memory facts live.
require lib/test.f
require lib/memory.f

package FETCH-TERM-TEST
public

NEWTYPE identity 0
CAST: >IDENTITY ( n -- identity )
CAST: IDENTITY>N ( identity -- n )

SUMTYPE value 0
   VARIANT empty ;VARIANT
   VARIANT number identity ;VARIANT
;SUMTYPE

SUMTYPE mapping 0
   VARIANT empty ;VARIANT
   VARIANT owned ptr u8 NUM:alloc-byte-len ;VARIANT
;SUMTYPE

private

1 TYPED-BUFFER VALUES value
DYNAMIC-BUFFER MAPPINGS mapping

: VALUE@ ( -- value ) 0 VALUES @ ;

: NUMBER@ ( -- n )
   VALUE@ MATCH value
      empty OF -1 ENDOF
      number OF IDENTITY>N ENDOF
   ;MATCH ;

: RELEASE ( -- )
   0 MAPPINGS @ MATCH mapping
      empty OF ENDOF
      owned OF MEM:RELEASE-BYTES ENDOF
   ;MATCH ;

: RUN ( -- )
   T-RESET
   construct value empty 0 VALUES !
   NUMBER@ -1 T=
   37 >IDENTITY construct value number 0 VALUES !
   NUMBER@ 37 T=
   1 MAPPINGS-RESERVE
   construct mapping empty 0 MAPPINGS !
   RELEASE
   8 MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES
   construct mapping owned 0 MAPPINGS !
   RELEASE
   MAPPINGS-RELEASE
   T-REPORT ;

RUN
;package

\ int-convert.f — the integer conversion words >U8, >U16, >U32 and >I64
\ (src/core/util.f), through the engine's own `:`.
\ Run: bin/hb --load test/int-convert.f   (exit 0 + "test: ok")

require src/lib/string.f
require src/lib/test.f

package INT-CONVERT-TEST

create DIAG 8192 allot

\ Captures the refusal diagnostic of one evaluated definition; answers its
\ throw code, 0 when the definition loaded.
: DEF-RC ( ptr u8 n -- n )
   DIAG 8192 DIAG-BUFFER!  TEST-EVAL:RC ;

: SAYS ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE  DIAG-BUFFER-OFF ;

: LOW8 ( -- u8 ) 300 >U8 ;
: LOW16 ( -- u16 ) 70000 >U16 ;
: LOW32 ( -- u32 ) -1 >U32 ;
: WIDE ( -- i64 ) -1 >I64 ;

T-RESET

\ Values: a wrong mask, a sign-extending mask or a conversion that changes the
\ cell answers another number.
LOW8 44 T=
LOW16 4464 T=
LOW32 $FFFFFFFF T=
WIDE -1 T=

\ Signatures: a word declared with a wider or different output would let the
\ result pass where its declared type is refused, or refuse a fitting use.
s" : P ( n -- u8 ) >U8 ;" DEF-RC 0 T=  DIAG-BUFFER-OFF
s" : X8 ( n -- bool ) >U8 ;" DEF-RC 70 T=  s" actual: u8" SAYS
s" : X16 ( n -- bool ) >U16 ;" DEF-RC 70 T=  s" actual: u16" SAYS
s" : X32 ( n -- bool ) >U32 ;" DEF-RC 70 T=  s" actual: u32" SAYS
s" : X64 ( n -- bool ) >I64 ;" DEF-RC 70 T=  s" actual: i64" SAYS
s" : Y8 ( bool -- ) >U8 drop ;" DEF-RC 70 T=  s" actual: bool" SAYS
s" : Y16 ( bool -- ) >U16 drop ;" DEF-RC 70 T=  s" actual: bool" SAYS
s" : Y32 ( bool -- ) >U32 drop ;" DEF-RC 70 T=  s" actual: bool" SAYS
s" : Y64 ( bool -- ) >I64 drop ;" DEF-RC 70 T=  s" actual: bool" SAYS

\ Retirement: a raw cast left defined retypes an n without masking it.
s" : R8 ( n -- u8 ) >U8-RAW ;" DEF-RC 70 T=  s" undefined word '>U8-RAW'" SAYS
s" : R16 ( n -- u16 ) >U16-RAW ;" DEF-RC 70 T=  s" undefined word '>U16-RAW'" SAYS
s" : R32 ( n -- u32 ) >U32-RAW ;" DEF-RC 70 T=  s" undefined word '>U32-RAW'" SAYS

T-REPORT

;package

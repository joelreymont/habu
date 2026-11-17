\ ffi-abi-test.f - target-independent FFI ABI and marshalling tests.
\ Run: bin/hb --load src/lib/ffi-abi-test.f

require src/lib/test.f
require src/lib/ffi-abi.f
require test/decl-diag-capture.f

package FFI-ABI-TEST
private

create FFI-T-OUT 1 cells allot

\ A part whose own `and` and `drop` spell the engine words a declared word
\ runs after a u32 result and after none: the declared words never call them.
package SHADOW
variable SEEN
: and ( n n -- n ) + ;
: drop ( n -- ) SEEN ! ;
PROCESS-SYMBOLS
FUNCTION: LOW labs ( n -- u32 ) ;FUNCTION
FUNCTION: GONE labs ( n -- ) ;FUNCTION
public
: GO ( -- n ) -6 GONE SEEN @ -5 LOW + ;
;package

: FFI-T-SHADOW ( -- )
   s" a declared word calls no word of its package" T-LABEL
   SHADOW:GO 5 T= ;

\ C writes through a staged cell: strtol stores its end pointer in argument 1,
\ an eight-byte output.
PROCESS-SYMBOLS
FUNCTION: FFI-T-STRTOL strtol ( ^u8 ^u8 n -- n ) ;FUNCTION
FUNCTION: FFI-T-LABS labs ( n -- n ) ;FUNCTION

: FFI-T-OUT-PARAM ( -- )
   s\" 42x\z" drop {: num:^ :}
   0 FFI-T-OUT !
   num FFI-T-OUT BYTE-VIEW 10 FFI-T-STRTOL 42 T=
   FFI-T-OUT @ num FFI:>CELL 2 + T= ;

\ The table these declarations grow lies in the storage the rollback cuts.
: FFI-T-CUT-TEXT$ ( -- ^u8 n )
   s" include test/fixtures/ffi-grow.f  1 throw" ;

: FFI-T-AFTER-TEXT$ ( -- ^u8 n )
   s\" FUNCTION: FFI-T-LLABS llabs ( n -- n ) ;FUNCTION  -7 FFI-T-LLABS" ;

: FFI-T-FILL ( n -- ) 0 ?do 777 , loop ;

: FFI-T-SELECT-TEXT$ ( -- ^u8 n )
   s" VERSIONED-LIBRARY habu-no-such 1  1 throw" ;

: FFI-T-ABS-TEXT$ ( -- ^u8 n )
   s" FUNCTION: FFI-T-ABS abs ( n -- i32 ) ;FUNCTION  -9 FFI-T-ABS" ;

\ Calls through rows declared before and after a rolled-back declaration.
: FFI-T-ROLLBACK ( -- )
   -5 FFI-T-LABS 5 T=
   FFI-T-CUT-TEXT$ TEST-EVAL:RC 1 T=
   4000 FFI-T-FILL
   -6 FFI-T-LABS 6 T=
   FFI-T-AFTER-TEXT$ TEST-EVAL:N 7 T=
   -8 FFI-T-LABS 8 T=
   s" a rolled-back library selection leaves the one before it" T-LABEL
   FFI-T-SELECT-TEXT$ TEST-EVAL:RC 1 T=
   FFI-T-ABS-TEXT$ TEST-EVAL:N 9 T= ;

\ A declaration's syntax error prints a record naming the token it read last.
: FFI-T-NO-DASHES$ ( -- ^u8 n )
   s" FUNCTION: FFI-T-NO-DASHES labs ( n n ) ;FUNCTION" ;

: FFI-T-NO-DASHES ( -- )
   s" an effect without '--' prints its record" T-LABEL
   DECL-DIAG:PROSE
   FFI-T-NO-DASHES$ TEST-EVAL:RC 70 T=
   s" error[E-FFI-SYNTAX]: expected '--' before ')'"
   DECL-DIAG:HAS? TTRUE
   s" = help: write the effect as ( inputs -- output )"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF ;

: FFI-T-NO-PATH ( -- )
   s" LIBRARY with no path prints its record" T-LABEL
   DECL-DIAG:PROSE
   s" LIBRARY" TEST-EVAL:RC 70 T=
   s" error[E-FFI-SYNTAX]: the library path is empty"
   DECL-DIAG:HAS? TTRUE
   s" = help: write the library's path or file name after LIBRARY"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF ;

\ A library name spelled in the other target's convention.
: FFI-T-FOREIGN$ ( -- ^u8 n )
   HB-TARGET-MACOS? if s" LIBRARY libx.so.5" exit then
   s" LIBRARY libx.5.dylib" ;

: FFI-T-FOREIGN-HEAD$ ( -- ^u8 n )
   HB-TARGET-MACOS? if
      s" error[E-FFI-LIBRARY]: library 'libx.so.5' is spelled for the other target"
      exit
   then
   s" error[E-FFI-LIBRARY]: library 'libx.5.dylib' is spelled for the other target" ;

: FFI-T-FOREIGN-HELP$ ( -- ^u8 n )
   HB-TARGET-MACOS? if
      s" = help: name it with VERSIONED-LIBRARY, which spells it for macos" exit
   then
   s" = help: name it with VERSIONED-LIBRARY, which spells it for linux" ;

: FFI-T-FOREIGN ( -- )
   s" LIBRARY with a name spelled for the other target prints its record" T-LABEL
   DECL-DIAG:PROSE
   FFI-T-FOREIGN$ TEST-EVAL:RC 70 T=
   FFI-T-FOREIGN-HEAD$ DECL-DIAG:HAS? TTRUE
   FFI-T-FOREIGN-HELP$ DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF ;

\ src/lib/ffi-test.f calls C functions that write the sret x8 and stack-slot
\ outputs, and names the shapes no C function witnesses.

: FFI-ABI-RUN ( -- )
   T-RESET
   FFI-T-OUT-PARAM
   FFI-T-ROLLBACK
   FFI-T-SHADOW
   FFI-T-NO-DASHES
   FFI-T-NO-PATH
   FFI-T-FOREIGN
   T-REPORT ;

FFI-ABI-RUN

;package

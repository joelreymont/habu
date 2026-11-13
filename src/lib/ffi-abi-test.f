\ ffi-abi-test.f - target-independent FFI ABI and marshalling tests.
\ Run: bin/hb --load src/lib/ffi-abi-test.f

require src/lib/test.f
require src/lib/ffi-abi.f

package FFI-ABI-TEST
private

create FFI-T-OUT 1 cells allot
create FFI-T-KP-CELL 1 cells allot
create FFI-T-KP-DST 2 cells allot

\ A kernel-parameter entry is an address handed to C as a cell.
CAST: FFI-T-CELL-AT ( n -- ptr n )

\ C writes through a staged cell: strtol stores its end pointer in argument 1,
\ an eight-byte output. memcpy copies the kernel-parameter block out whole.
PROCESS-SYMBOLS
FUNCTION: FFI-T-STRTOL strtol ( ptr u8 ptr u8 n -- n ) 1 8 WRITES-BYTES ;FUNCTION
FUNCTION: FFI-T-MEMCPY memcpy ( ptr u8 ptr u8 n -- n ) 0 2 WRITES-ARG ;FUNCTION
FUNCTION: FFI-T-LABS labs ( n -- n ) ;FUNCTION

: FFI-T-OUT-PARAM ( -- )
   s\" 42x\z" drop {: num:ptr :}
   0 FFI-T-OUT FFI:OUT!
   num FFI-T-OUT BYTE-VIEW 10 FFI-T-STRTOL 42 T=
   FFI-T-OUT FFI:OUT@ num FFI:>CELL 2 + T= ;

: FFI-T-KPARAM-CAP ( -- )
   FFI:KPARAM-RESET
   [: 17 0 do i FFI:KPARAM-VALUE+ loop ;] E-FFI-ARITY TTHROWSQ ;

\ EVALREC-CELL (src/habu/layout.f) must lie outside the FFI task-DATA block
\ [FFI:BUF-OFF, FFI:KPARAM-END-OFF): an FFI call fills that block, and an overlap
\ sends a throw that crosses an evaluate boundary to a data address.
: FFI-T-EVALREC-DISJOINT ( -- )
   EVALREC-CELL FFI:BUF-OFF <
   EVALREC-CELL FFI:KPARAM-END-OFF >= or TTRUE ;

\ The first entry points at the FFI-owned cell holding 13, the second at the
\ caller's cell.
: FFI-T-KPARAMS ( -- )
   FFI:KPARAM-RESET
   13 FFI:KPARAM-VALUE+
   21 FFI-T-KP-CELL FFI:OUT!
   FFI-T-KP-CELL FFI:KPARAM+
   FFI:KPARAM-COUNT 2 T=
   FFI-T-KP-DST BYTE-VIEW FFI:KPARAMS drop BYTE-VIEW 16 FFI-T-MEMCPY drop
   FFI-T-KP-DST @ FFI-T-CELL-AT @ 13 T=
   FFI-T-KP-DST 1 cells + @ FFI-T-KP-CELL FFI:>CELL T=
   FFI-T-KPARAM-CAP
   FFI:KPARAM-RESET
   FFI:KPARAM-COUNT 0 T= ;

\ A row no other declaration shares: k's two base-16 digits are its argument
\ and spill counts.
variable FFI-T-PADS
: FFI-T-PAD ( -- n )
   FFI-T-PADS @ {: k:n :}
   1 FFI-T-PADS +!
   s" hb_pad_row" 0
   k 16 /mod  0
   FFI:DECLARE-ROW 1+ ;

: FFI-T-POW2? ( n -- bool ) dup 1- and 0= ;

\ A full table, so the next declaration grows it.
: FFI-T-FULL ( -- )
   begin FFI-T-PAD dup FFI-T-POW2? 0= while drop repeat drop ;

\ The table this declaration grows lies in the storage its rollback cuts.
: FFI-T-CUT-TEXT$ ( -- ptr u8 n )
   s\" FUNCTION: FFI-T-CUT getpid ( -- i32 ) ;FUNCTION  1 throw" ;

: FFI-T-AFTER-TEXT$ ( -- ptr u8 n )
   s\" FUNCTION: FFI-T-LLABS llabs ( n -- n ) ;FUNCTION  -7 FFI-T-LLABS" ;

: FFI-T-FILL ( n -- ) 0 ?do 777 , loop ;

: FFI-T-SELECT-TEXT$ ( -- ptr u8 n )
   s" VERSIONED-LIBRARY habu-no-such 1  1 throw" ;

: FFI-T-ABS-TEXT$ ( -- ptr u8 n )
   s" FUNCTION: FFI-T-ABS abs ( n -- i32 ) ;FUNCTION  -9 FFI-T-ABS" ;

\ Calls through rows declared before and after a rolled-back declaration.
: FFI-T-ROLLBACK ( -- )
   -5 FFI-T-LABS 5 T=
   FFI-T-FULL
   FFI-T-CUT-TEXT$ TEST-EVAL:RC 1 T=
   4000 FFI-T-FILL
   -6 FFI-T-LABS 6 T=
   FFI-T-AFTER-TEXT$ TEST-EVAL:N 7 T=
   -8 FFI-T-LABS 8 T=
   s" a rolled-back library selection leaves the one before it" T-LABEL
   FFI-T-SELECT-TEXT$ TEST-EVAL:RC 1 T=
   FFI-T-ABS-TEXT$ TEST-EVAL:N 9 T= ;

\ src/lib/ffi-test.f calls C functions that write the sret x8 and stack-slot
\ outputs, and names the shapes no C function witnesses.

: FFI-ABI-RUN ( -- )
   T-RESET
   FFI-T-EVALREC-DISJOINT
   FFI-T-OUT-PARAM
   FFI-T-KPARAMS
   FFI-T-ROLLBACK
   T-REPORT ;

FFI-ABI-RUN

;package

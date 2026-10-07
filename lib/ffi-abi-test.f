\ ffi-abi-test.f - target-independent FFI ABI and marshalling tests.
\ Run: bin/hb --load lib/ffi-abi-test.f

require lib/test.f
require lib/ffi-abi.f

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

: FFI-T-OUT-PARAM ( -- )
   s\" 42x\z" drop {: num:ptr :}
   0 FFI-T-OUT FFI:OUT!
   num FFI-T-OUT BYTE-VIEW 10 FFI-T-STRTOL 42 T=
   FFI-T-OUT FFI:OUT@ num FFI:>CELL 2 + T=
   [: FFI-T-OUT 0 0 FFI:WRITABLE! ;] E-FFI-ARITY TTHROWSQ ;

: FFI-T-KPARAM-CAP ( -- )
   FFI:KPARAM-RESET
   [: 17 0 do i FFI:KPARAM-VALUE+ loop ;] E-FFI-ARITY TTHROWSQ ;

\ Regression: the evaluate throw-unwind cell (EVALREC-CELL, src/habu/layout.f) must
\ live outside the FFI task-DATA block [FFI:BUF-OFF, FFI:KPARAM-END-OFF). An
\ FFI call fills that block, so an overlap silently clobbers the branch target and a
\ throw crossing an evaluate boundary (any FFI-using program run under include) jumps
\ to a data address. Assert the two regions are disjoint at build time.
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

\ The sret x8 and stack-slot outputs have no C function that writes them on
\ both ABIs: test/ffi-stub-child.f covers them with fixed stubs, and
\ lib/ffi-test.f runs that child.

: FFI-ABI-RUN ( -- )
   T-RESET
   FFI-T-EVALREC-DISJOINT
   FFI-T-OUT-PARAM
   FFI-T-KPARAMS
   T-REPORT ;

FFI-ABI-RUN

;package

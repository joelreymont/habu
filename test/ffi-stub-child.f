\ ffi-stub-child.f - the FFI staging shapes no C function witnesses on both
\ ABIs, called against fixed AArch64 stubs. lib/ffi-test.f runs it as a window
\ child: test/native-window-owner-child.f loads it after
\ test/mcode-window-prepare.f, whose owner words publish the stubs and call
\ them. It prints `test: ok`, then the window `window: 0`.
\
\ An sret result (x8) has no non-variadic libc witness, and the stack spills
\ past the argument registers come only from variadic callees, whose every
\ variadic argument Apple's ABI puts on the stack. So these cases keep their
\ exact instruction words. A minter writes them little-endian into CODE and
\ appends them at the free code slot, which the publication then moves past, so
\ the bytes stay put for the call that follows. A minter runs inside a
\ definition: a top-level one would write over the line being interpreted.

require lib/test.f
require lib/le.f
require lib/ffi-abi.f

package FFI-STUB

64 BUFFER: CODE
variable CODE-U
create OUT 1 cells allot

: INSN ( n -- )
   CODE CODE-U @ + LE:U32!
   CODE-U @ 4 + CODE-U ! ;

\ Publish the words INSN wrote at the free code slot and answer the address.
: MINT ( -- n )
   cp@ {: fn:n :}
   CODE fn CODE-U @ NPUB:WRITE-AT
   0 CODE-U !
   fn ;

\ x0 = x0+..+x7 + [sp+0] + [sp+8]; ret.
: SUM10 ( -- n )
   $8B010000 INSN  $8B020000 INSN  $8B030000 INSN  $8B040000 INSN
   $8B050000 INSN  $8B060000 INSN  $8B070000 INSN
   $F94003E9 INSN  $8B090000 INSN
   $F94007E9 INSN  $8B090000 INSN
   $D65F03C0 INSN  MINT ;

\ d0 = d0 + the double in stack slot 0; ret.
: FADD-FSTACK ( -- n )
   $F94003E9 INSN  $9E670128 INSN  $1E682800 INSN  $D65F03C0 INSN  MINT ;

\ [x8] = x0; ret.
: X8-STORE ( -- n )
   $F9000100 INSN  $D65F03C0 INSN  MINT ;

\ [[sp+0]] = x0; ret.
: STACK-STORE ( -- n )
   $F94003E9 INSN  $F9000120 INSN  $D65F03C0 INSN  MINT ;

\ Exact ten-integer binding covers x0-x7 and two stack-spilled cells.
: SUM10-CALL ( -- n )
   FFI:RESET
   10 0 ?do i 1+ i FFI:VALUE! loop
   10 SUM10 FFI:CALL-AT ;

\ Exact floating-register plus stack-spill binding with distinct extents.
: FADD-FSTACK-CALL ( -- r )
   FFI:RESET
   1.25 0 FFI:FLOAT!
   2.75 0 FFI:STACK-FLOAT!
   1 FADD-FSTACK FFI:CALL-ABI-R-AT ;

\ Exact sret binding fixes x8 to an eight-byte output.
: X8-CALL ( ptr a -- n )
   {: out:ptr :}
   FFI:RESET
   42 0 FFI:VALUE!
   out 8 FFI:X8-WRITABLE!
   0 X8-STORE FFI:CALL-ABI-AT ;

\ Exact mixed-ABI call fixes stack slot zero as an eight-byte output.
: STACK-CALL ( ptr a -- n )
   {: out:ptr :}
   FFI:RESET
   77 0 FFI:VALUE!
   out 8 0 FFI:STACK-WRITABLE!
   1 STACK-STORE FFI:CALL-ABI-AT ;

public

: RUN ( -- )
   T-RESET
   SUM10-CALL 55 T=
   FADD-FSTACK-CALL 4.0 f= T-ASSERT
   0 OUT FFI:OUT!
   OUT X8-CALL drop
   OUT @ 42 T=
   0 OUT FFI:OUT!
   OUT STACK-CALL drop
   OUT @ 77 T=
   T-REPORT ;

;package

FFI-STUB:RUN

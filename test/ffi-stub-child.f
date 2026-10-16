\ ffi-stub-child.f - the FFI staging shapes no C function witnesses on both
\ ABIs, called against fixed AArch64 stubs. src/lib/ffi-test.f runs it as a window
\ child: test/native-window-owner-child.f loads it after
\ test/mcode-window-prepare.f, whose owner words publish the stubs and call
\ them. It prints `test: ok`, then the window `window: 0`.
\
\ An sret result (x8) has no non-variadic libc witness, and the stack spills
\ past the argument registers come only from variadic callees, whose every
\ variadic argument Apple's ABI puts on the stack. No libm function takes a
\ binary32 beside a double and an integer, eight binary32 beside eight
\ integers, or a binary32 register before a stack double. So these cases keep
\ their exact instruction words. A minter writes them little-endian into CODE
\ and appends them at the free code slot, which the publication then moves past,
\ so the bytes stay put for the call that follows. A minter runs inside a
\ definition: a top-level one would write over the line being interpreted.

require src/lib/string.f
require src/lib/test.f
require src/lib/le.f
require src/lib/ffi-abi.f

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

\ d0 = d0 + the double in stack slot 0; ret. The sum goes through d1, which
\ AAPCS64 lets a callee clobber; d8-d15 it must preserve.
: FADD-FSTACK ( -- n )
   $F94003E9 INSN  $9E670121 INSN  $1E612800 INSN  $D65F03C0 INSN  MINT ;

\ [x8] = x0; ret.
: X8-STORE ( -- n )
   $F9000100 INSN  $D65F03C0 INSN  MINT ;

\ [[sp+0]] = x0; ret.
: STACK-STORE ( -- n )
   $F94003E9 INSN  $F9000120 INSN  $D65F03C0 INSN  MINT ;

\ d0 = (double)s2 + (double)s0 + d1 + (double)x0; ret: a float beside a double
\ and an integer, binary32 in the low half of its slot.
: F32-MIX ( -- n )
   $1E22C043 INSN  $1E22C000 INSN  $1E612800 INSN  $1E632800 INSN
   $9E620001 INSN  $1E612800 INSN  $D65F03C0 INSN  MINT ;
\ (fcvt d3,s2; fcvt d0,s0; fadd d0,d0,d1; fadd d0,d0,d3; scvtf d1,x0; fadd d0,d0,d1; ret)

\ d0 = (double)s7 + (double)x7; ret: the last of eight floats beside eight integers.
: F32-LAST ( -- n )
   $1E22C0E0 INSN  $9E6200E1 INSN  $1E612800 INSN  $D65F03C0 INSN  MINT ;
\ (fcvt d0,s7; scvtf d1,x7; fadd d0,d0,d1; ret)

\ d0 = (double)s0 + the double in stack slot 0; ret: a float register beside a
\ float stack slot, a fixed f32 before Apple's variadic doubles.
: F32-FSTACK ( -- n )
   $F94003E9 INSN  $9E670121 INSN  $1E22C000 INSN  $1E612800 INSN  $D65F03C0 INSN  MINT ;
\ (ldr x9,[sp]; fmov d1,x9; fcvt d0,s0; fadd d0,d0,d1; ret)

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

: F32-MIX-CALL ( -- r )
   FFI:RESET
   16777217.0 0 FFI:FLOAT32!  3.125 1 FFI:FLOAT!  7 0 FFI:VALUE!  -0.5 2 FFI:FLOAT32!
   0 F32-MIX FFI:CALL-ABI-R-AT ;        \ 16777216 + 3.125 - 0.5 + 7
: F32-LAST-CALL ( -- r )
   FFI:RESET
   8 0 ?do  i 10 +  i FFI:VALUE!  loop
   0.5 0 FFI:FLOAT32!  1.5 1 FFI:FLOAT32!  2.5 2 FFI:FLOAT32!  3.5 3 FFI:FLOAT32!
   4.5 4 FFI:FLOAT32!  5.5 5 FFI:FLOAT32!  6.5 6 FFI:FLOAT32!  7.5 7 FFI:FLOAT32!
   0 F32-LAST FFI:CALL-ABI-R-AT ;       \ 17 + 7.5
: F32-FSTACK-CALL ( -- r )
   FFI:RESET
   1.25 0 FFI:FLOAT32!  2.5 0 FFI:STACK-FLOAT!
   1 F32-FSTACK FFI:CALL-ABI-R-AT ;     \ 1.25 + 2.5

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
   F32-MIX-CALL 16777225.625 f= T-ASSERT
   F32-LAST-CALL 24.5 f= T-ASSERT
   F32-FSTACK-CALL 3.75 f= T-ASSERT
   T-REPORT ;

;package

FFI-STUB:RUN

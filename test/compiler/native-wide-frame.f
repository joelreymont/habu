\ native-wide-frame.f - a frame past the reach of one add/sub immediate.
\
\ A routine takes its frame by moving the stack pointer, and the add/sub
\ immediate holds 0 .. 4095. The frame contract is wider: src/arch/arm64/
\ machine.f FRAME-MAX admits every frame the scaled slot offset reaches, 32752
\ bytes, and the allocator hands out slots up to it. So a frame of a page or
\ more is reserved and released in two words, the pages over the shifted
\ immediate and the rest over the plain one (src/compiler/native/emit.f
\ WORD-RESERVE).
\
\ Sixty locals stay live across the call in each of a run of conditionals, and
\ no value survives a call in a register, so every conditional adds its copies
\ of the locals to the frame. Sixty-four of them put the frame in the top page
\ the contract admits. The word must compile, take every call and give back
\ the sum of its locals. Its frame is read back, so the case still reaches the
\ range it names, and so is its code: the frame it takes and gives back must be
\ the frame the allocator measured.
require lib/test.f
require src/habu/code-bytes.f

1 set-tier

package NWF-FIXTURE
public

variable V
: TICK ( -- ) 1 V +! ;

: WIDE ( -- n )
   V @ 0 + {: a0:n :} V @ 1 + {: a1:n :} V @ 2 + {: a2:n :} V @ 3 + {: a3:n :}
   V @ 4 + {: a4:n :} V @ 5 + {: a5:n :} V @ 6 + {: a6:n :} V @ 7 + {: a7:n :}
   V @ 8 + {: a8:n :} V @ 9 + {: a9:n :} V @ 10 + {: a10:n :}
   V @ 11 + {: a11:n :} V @ 12 + {: a12:n :} V @ 13 + {: a13:n :}
   V @ 14 + {: a14:n :} V @ 15 + {: a15:n :} V @ 16 + {: a16:n :}
   V @ 17 + {: a17:n :} V @ 18 + {: a18:n :} V @ 19 + {: a19:n :}
   V @ 20 + {: a20:n :} V @ 21 + {: a21:n :} V @ 22 + {: a22:n :}
   V @ 23 + {: a23:n :} V @ 24 + {: a24:n :} V @ 25 + {: a25:n :}
   V @ 26 + {: a26:n :} V @ 27 + {: a27:n :} V @ 28 + {: a28:n :}
   V @ 29 + {: a29:n :} V @ 30 + {: a30:n :} V @ 31 + {: a31:n :}
   V @ 32 + {: a32:n :} V @ 33 + {: a33:n :} V @ 34 + {: a34:n :}
   V @ 35 + {: a35:n :} V @ 36 + {: a36:n :} V @ 37 + {: a37:n :}
   V @ 38 + {: a38:n :} V @ 39 + {: a39:n :} V @ 40 + {: a40:n :}
   V @ 41 + {: a41:n :} V @ 42 + {: a42:n :} V @ 43 + {: a43:n :}
   V @ 44 + {: a44:n :} V @ 45 + {: a45:n :} V @ 46 + {: a46:n :}
   V @ 47 + {: a47:n :} V @ 48 + {: a48:n :} V @ 49 + {: a49:n :}
   V @ 50 + {: a50:n :} V @ 51 + {: a51:n :} V @ 52 + {: a52:n :}
   V @ 53 + {: a53:n :} V @ 54 + {: a54:n :} V @ 55 + {: a55:n :}
   V @ 56 + {: a56:n :} V @ 57 + {: a57:n :} V @ 58 + {: a58:n :}
   V @ 59 + {: a59:n :}
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then V @ 0 > if TICK then V @ 0 > if TICK then
   V @ 0 > if TICK then
   a0 a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 +
   a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 +
   a26 + a27 + a28 + a29 + a30 + a31 + a32 + a33 + a34 + a35 + a36 + a37 +
   a38 + a39 + a40 + a41 + a42 + a43 + a44 + a45 + a46 + a47 + a48 + a49 +
   a50 + a51 + a52 + a53 + a54 + a55 + a56 + a57 + a58 + a59 + ;
A64RA:FRAME constant WIDE-FRAME

;package

package NWF-TEST
private

\ `sub sp, sp, #imm` and `add sp, sp, #imm`, plain and with `lsl #12`, as the
\ architecture encodes them: the immediate is bits 10..21.
$D10003FF constant SUB-SP
$D14003FF constant SUB-SP-PAGES
$910003FF constant ADD-SP
$914003FF constant ADD-SP-PAGES
: SP-WORD ( n n -- n ) 10 lshift or ;

: CODE-WORD ( ptr u8 n -- n ) {: a:ptr k:n :}
   k 4 * {: o:n :}
   a o + c@
   a o 1+ + c@ 8 lshift or
   a o 2 + + c@ 16 lshift or
   a o 3 + + c@ 24 lshift or ;

: HAS-WORD? ( n -- bool ) {: w:n :}
   s" NWF-FIXTURE:WIDE" XREF-FIND {: rec:ptr :}
   rec XREF-START rec XREF-CODE-BYTES CODE-BYTES:AT {: a:ptr u :}
   false
   u 4 / 0 ?do a i CODE-WORD w = if drop true leave then loop ;

: WIDE-CASE ( -- )
   s" a frame in the contract's top page compiles" T-LABEL
   NWF-FIXTURE:WIDE-FRAME 7 12 lshift >= TTRUE
   NWF-FIXTURE:WIDE-FRAME A64M:FRAME-MAX <= TTRUE
   s" its frame is taken and given back as whole pages and the rest" T-LABEL
   NWF-FIXTURE:WIDE-FRAME 12 rshift {: pages:n :}
   NWF-FIXTURE:WIDE-FRAME 4095 and {: rest:n :}
   SUB-SP-PAGES pages SP-WORD HAS-WORD? TTRUE
   SUB-SP rest SP-WORD HAS-WORD? TTRUE
   ADD-SP rest SP-WORD HAS-WORD? TTRUE
   ADD-SP-PAGES pages SP-WORD HAS-WORD? TTRUE
   s" and runs: every call taken, every local read back" T-LABEL
   1 NWF-FIXTURE:V !
   NWF-FIXTURE:WIDE 60 1770 + T=
   NWF-FIXTURE:V @ 1 64 + T= ;

public

: RUN ( -- )
   WIDE-CASE ;

;package

T-RESET
NWF-TEST:RUN
T-REPORT

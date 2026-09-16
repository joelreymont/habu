\ a64-indexed.f - the writeback addressing modes of the ARM64 assembler.
\
\ ARM64 spells a load or a store that also moves its base register as ONE
\ instruction, in two modes: post-index transfers at the base and then adds the
\ offset to it, pre-index adds first and transfers at the new address. They are
\ what a data-stack push, a data-stack pop and a link-register frame are, and
\ each of those was two instructions here until they arrived.
\
\ The expected words below were produced by an independent assembler - written
\ as bytes and read back with `objdump -b binary -m aarch64 -D` - before they
\ were written down, so no row can agree with a bug in the Habu encoders by
\ construction. The Gforth recovery seed reached the same two base constants
\ independently (`bootstrap/cg/asm.fs` ENC-LDRPO $F8400400, ENC-STRPR
\ $F8000C00), which is a third reading of the same bits.
\
\ WHY THE THREE MODES ARE ALSO COMPARED WITH EACH OTHER. A table of golden
\ words catches an encoder that changed; it does not say what makes these
\ encoders different from the plain-offset LDUR/STUR they are one base constant
\ away from. A copied base or a transposed pair of mode bits would be a real
\ miscompile - a push that never moves the pointer, or a pop that moves it the
\ wrong way - so MODE-CASES asserts the three modes differ from each other in
\ exactly the two bits that name the mode, on identical operands.
\
\ THE OFFSET BOUND IS NOT NEW. Both modes carry the same signed nine-bit byte
\ field `?SIMM9` screens for LDUR and STUR, whose two ends and whose refusals
\ are already rows in test/compiler/insn-schema.f. These forms inherit that
\ screen rather than adding one, so the ends are asserted here and the refusals
\ stay where they are pinned.

require lib/test.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

package A64-INDEXED-TEST
\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f).
using A64ASM
private

31 constant SP-REG
19 constant DS-REG                 \ the engine's data-stack pointer

\ The bits that name the addressing mode, 11:10 of a load/store with a signed
\ nine-bit offset: 00 plain, 01 post-index, 11 pre-index.
$00000C00 constant MODE-BITS

: MODE-OF ( n -- n )  MODE-BITS and ;

: SANS-MODE ( n -- n )  MODE-BITS invert and ;

\ Read one emitted word back out of the code buffer, through the accessor the
\ assembler itself writes through.
: U32@ ( n -- n )
   CW@ dup c@
   swap 1 CODE-BYTE+ dup c@ $8 lshift
   swap 1 CODE-BYTE+ dup c@ $10 lshift
   swap 1 CODE-BYTE+ c@ $18 lshift
   or or or ;

\ ---- the golden words --------------------------------------------------------
\ The four the engine and the compiler emit, then both ends of the field.

: FRAME-CASES ( -- )
   30 SP-REG -16 ENC-STRPRE  $F81F0FFE T=          \ str x30,[sp,#-16]!
   30 SP-REG 16 ENC-LDRPOST  $F84107FE T= ;        \ ldr x30,[sp],#16

: STACK-CASES ( -- )
   9 DS-REG 8 ENC-STRPOST   $F8008669 T=           \ str x9,[x19],#8
   9 DS-REG -8 ENC-LDRPRE   $F85F8E69 T=           \ ldr x9,[x19,#-8]!
   16 DS-REG 8 ENC-STRPOST  $F8008670 T=
   16 DS-REG -8 ENC-LDRPRE  $F85F8E70 T=
   0 DS-REG 8 ENC-STRPOST   $F8008660 T=
   0 DS-REG -8 ENC-LDRPRE   $F85F8E60 T= ;

\ The D file is the same two modes one opcode bit apart, bit 26, which says the
\ transferred register is of the SIMD&FP file. d18 is an ordinary D register.
: FLOAT-CASES ( -- )
   0 DS-REG 8 ENC-STRDPOST  $FC008660 T=           \ str d0,[x19],#8
   0 DS-REG -8 ENC-LDRDPRE  $FC5F8E60 T=           \ ldr d0,[x19,#-8]!
   18 DS-REG 8 ENC-STRDPOST $FC008672 T=
   18 DS-REG -8 ENC-LDRDPRE $FC5F8E72 T= ;

\ Both ends of the signed nine-bit byte field, in both modes.
: FIELD-END-CASES ( -- )
   1 2 255 ENC-STRPOST   $F80FF441 T=
   1 2 -256 ENC-STRPOST  $F8100441 T=
   3 4 255 ENC-LDRPRE    $F84FFC83 T=
   3 4 -256 ENC-LDRPRE   $F8500C83 T= ;

\ ---- what makes the modes different ------------------------------------------
\ Same operands through all three encoders: the words must agree everywhere but
\ the two mode bits, and each mode bit pattern must be its own.

: MODE-CASES ( -- )
   9 DS-REG 8 ENC-STUR {: plain:n :}
   9 DS-REG 8 ENC-STRPOST {: post:n :}
   9 DS-REG 8 ENC-STRPRE {: pre:n :}
   plain SANS-MODE post SANS-MODE T=
   plain SANS-MODE pre SANS-MODE T=
   plain MODE-OF $000 T=
   post MODE-OF $400 T=
   pre MODE-OF $C00 T=
   9 DS-REG -8 ENC-LDUR {: lplain:n :}
   9 DS-REG -8 ENC-LDRPOST {: lpost:n :}
   9 DS-REG -8 ENC-LDRPRE {: lpre:n :}
   lplain SANS-MODE lpost SANS-MODE T=
   lplain SANS-MODE lpre SANS-MODE T=
   lplain MODE-OF $000 T=
   lpost MODE-OF $400 T=
   lpre MODE-OF $C00 T= ;

\ ---- through the shipped mnemonic and the shipped store ----------------------
\ The encoders above answer numbers. These two put the number in the code
\ buffer through `EMITW`, which is what an emitter calling `STRPOST,` gets.

: MNEMONIC-CASES ( -- )
   ARESET
   9 DS-REG 8 STRPOST,
   0 U32@ $F8008669 T=
   ARESET
   9 DS-REG -8 LDRPRE,
   0 U32@ $F85F8E69 T=
   ASM-LEN 4 T= ;

public

: RUN ( -- )
   FRAME-CASES
   STACK-CASES
   FLOAT-CASES
   FIELD-END-CASES
   MODE-CASES
   MNEMONIC-CASES ;

;package

T-RESET
A64-INDEXED-TEST:RUN
T-REPORT

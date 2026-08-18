\ judge/traffic.f - how often a compiled routine touches the CALLER's own data
\ stack. One concern: counting that traffic in a live word's emitted code.
\
\ WHY THE JUDGE COUNTS THIS AND NOT A TIME. What separates the two code
\ generators is where an intermediate value lives: the engine moves every one of
\ them through a stack slot in memory, and the chain holds it in a register and
\ touches the slot only at the edges of the routine. That difference is what the
\ timings showed. Unlike a timing it is EXACT, it moves for one reason, and it
\ does not turn on host load - so it can sit in the judged artifact beside the
\ byte counts and be compared against a committed file, which a cost never can.
\
\ BE CLEAR ABOUT WHAT IT IS NOT. It is not the cost claim restated. A chain that
\ made the same number of accesses and took twice as long would pass every row.
\ The cost claim itself is tools/judge-timed.f, run by hand on a quiet machine.
\
\ FOUR SPELLINGS OF ONE ACCESS, which is why the count adds eight forms and not
\ two. Two of them are the addressing mode: the chain stands its data-stack
\ pointer where the fewest adjustments are needed, so a cell it reaches can be
\ UNDER the pointer as well as over it - and under it is the unscaled signed
\ form, Ldur and Stur, a different encoding of the same access. The other two
\ are the REGISTER FILE: a double reaches and leaves a slot in the file it lives
\ in, so the same eight bytes travel under the SIMD&FP spelling of the same two
\ modes. A counter that knew only some of the four would report an access the
\ routine really makes as no access at all, and would read a change of
\ addressing mode - or of register file - as a saving.
\
\ WHICH REGISTER IS THE DATA STACK IS NOT SPELLED OUT HERE. It is read from
\ src/compiler/a64-effect.f DSTACK-GPR, which is the chain's own statement of
\ where the running engine keeps that pointer. A second copy of that number
\ would be a second thing to keep in step.
\
\ AND THE WALK OVER THE CODE IS NOT THIS FILE'S EITHER. tools/codegen-tail-probe.f
\ owns reading a live word's own dictionary record - where its code starts, how
\ far it runs, and the instruction at each step - and every other reader of
\ emitted code in this repository goes through it. A name nothing published is
\ refused there rather than answered with a count of zero, which is what a
\ silently missing routine would otherwise look like.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require src/compiler/a64-effect.f
require tools/codegen-tail-probe.f

package JUDGE-TRAFFIC

private

$FFC00000 constant MEM-MASK
$F9000000 constant STR-OP          \ str  Xt, [Xn, #imm]   - scaled unsigned
$F9400000 constant LDR-OP          \ ldr  Xt, [Xn, #imm]
$F8000000 constant STUR-OP         \ stur Xt, [Xn, #simm]  - unscaled signed
$F8400000 constant LDUR-OP         \ ldur Xt, [Xn, #simm]
$FD000000 constant STRD-OP         \ str  Dt, [Xn, #imm]   - the SIMD&FP file
$FD400000 constant LDRD-OP         \ ldr  Dt, [Xn, #imm]
$FC000000 constant STURD-OP        \ stur Dt, [Xn, #simm]
$FC400000 constant LDURD-OP        \ ldur Dt, [Xn, #simm]

5 constant RN-SHIFT
$1F constant REG-MASK

: FORM? ( n n -- bool ) {: w:n op:n :}
   w MEM-MASK and op =
   w RN-SHIFT rshift REG-MASK and  A64EFF:DSTACK-GPR =  and ;

public

\ Is this one instruction word an access to the caller's data stack? Public
\ because it is what the fixtures attack: a count over a real routine can only
\ be checked against another count, while this can be handed the exact encodings
\ a miss would look like.
: INSN? ( n -- bool ) {: w:n :}
   w STR-OP FORM? if true exit then
   w LDR-OP FORM? if true exit then
   w STUR-OP FORM? if true exit then
   w LDUR-OP FORM? if true exit then
   w STRD-OP FORM? if true exit then
   w LDRD-OP FORM? if true exit then
   w STURD-OP FORM? if true exit then
   w LDURD-OP FORM? ;

\ How many such accesses a live word's own compiled code makes.
: COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u NTAILPROBE:INSNS 0 ?do
      a u i NTAILPROBE:INSN@ INSN? if 1+ then
   loop ;

;package

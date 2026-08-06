\ branch.f - the branch-with-link instruction, read and written. One concern:
\ the one arithmetic that turns a `bl` into an address and an address back into
\ a `bl`.
\
\ WHY THIS IS A FILE AND NOT THREE PRIVATE COPIES. Three seams of this system
\ have to know where a call instruction goes. The publication seam reads the
\ branches of an emission it is about to write, so that what a routine destroys
\ covers what everything it calls destroys (src/compiler/native/publish.f,
\ BRANCH-CK). The workload scan reads the branches of every live record, to
\ answer who calls a word and who carries a copy of its body instead
\ (tools/codegen-workload-scan.f). The redirection seam reads them AND writes
\ them, because moving a caller onto a word's new code is exactly rewriting the
\ displacement (src/compiler/native/reach.f). Each of the three had its own
\ mask, its own sign test and its own multiplication, and a fourth was about to
\ be written. Three copies of one arithmetic is three chances for one of them to
\ drift: a sign test that stopped sign-extending would make the scan report a
\ caller as not calling and the seam refuse a redirection that was fine, and the
\ two would still agree with each other while disagreeing with the machine.
\
\ THE FORM. AArch64 encodes `bl` as the six bits $94000000 followed by a signed
\ twenty-six-bit count of INSTRUCTIONS from the instruction's own address. So
\ the target is the site plus four times a sign-extended field, and the field is
\ the distance in instructions - which is why both addresses have to be whole
\ instructions and why the distance has a limit. Both are refusals here rather
\ than assumptions: an address that is not instruction aligned would encode a
\ displacement that lands somewhere else entirely, and a distance past the field
\ would silently wrap around to a target inside the code region.
\
\ WHAT IT DOES NOT DECIDE. Whether the instruction it is being handed is
\ reachable, whether the code at the target is anything anybody should call, and
\ whether writing one is sound. Those belong to the seams above, which is why
\ this file has one refusal and no state.

require lib/prelude.f
require lib/errors.f

package NBR

public

4 constant INSN-BYTES

private

$FC000000 constant BL-MASK
$94000000 constant BL-OP
$14000000 constant B-OP
$03FFFFFF constant IMM26
$02000000 constant IMM26-SIGN         \ the top bit of the twenty-six-bit field
$04000000 constant IMM26-SPAN         \ two to the twenty-sixth, the wrap the sign is taken against

\ The largest and smallest instruction counts the field holds, as counts rather
\ than as byte distances, because the field counts instructions. They are the
\ two halves of the same twenty-six-bit span and are written in decimal, because
\ the negative one is a value and not a bit pattern.
33554431 constant IMM26-MAX
-33554432 constant IMM26-MIN

: ALIGNED? ( n -- bool )
   INSN-BYTES mod 0= ;

public

\ Is this instruction word a branch-with-link?
: BL? ( n -- bool )
   BL-MASK and BL-OP = ;

\ And is it the plain unconditional branch? The two forms differ in one bit of
\ the opcode and in nothing else: same mask, same twenty-six-bit signed count of
\ instructions, so everything below serves both and the file stays the one
\ reader of that displacement.
: B? ( n -- bool )
   BL-MASK and B-OP = ;

private

\ Where the branch at this address goes. The field is a signed count of
\ instructions from the site itself, so the answer is arithmetic on the site's
\ address and not a guess. Both forms share it, which is why it is written once.
: TARGET ( n n -- n ) {: at:n w:n :}
   w IMM26 and {: d:n :}
   d IMM26-SIGN and 0<> if d IMM26-SPAN - INSN-BYTES * at + exit then
   d INSN-BYTES * at + ;

public

: BL-TARGET ( n n -- n )
   TARGET ;

: B-TARGET ( n n -- n )
   TARGET ;

\ Could a branch-with-link at this address name this target at all? Both
\ addresses have to be whole instructions and the distance between them has to
\ fit the field. A caller that has to refuse rather than throw asks this; the
\ builder below asks it again and throws, because a wrong answer here is an
\ instruction that branches into the middle of somewhere.
: REACHES? ( n n -- bool ) {: at:n target:n :}
   at ALIGNED? 0= if false exit then
   target ALIGNED? 0= if false exit then
   target at - INSN-BYTES / {: d:n :}
   d IMM26-MAX > if false exit then
   d IMM26-MIN < if false exit then
   true ;

\ The instruction word a branch-with-link at this address needs in order to
\ enter this target.
: BL-WORD ( n n -- n ) {: at:n target:n :}
   at target REACHES? 0= if E-NBR-RANGE throw then
   target at - INSN-BYTES /  IMM26 and  BL-OP or ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package

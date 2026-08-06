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

\ ---- the conditional forms ----------------------------------------------------
\ The three conditional branches this chain emits carry a NINETEEN-bit signed
\ count of instructions instead of the twenty-six the two forms above use, and
\ the count sits five bits up rather than at the bottom. Everything else about
\ the arithmetic is the same, so it is written here beside the other one for the
\ reason this whole file exists: a reader that needs where a conditional branch
\ goes should not be the fourth place that sign-extends a displacement.
\
\ WHO ASKS. Anything that has to follow control flow through emitted code rather
\ than merely recognise a call - tools/codegen-loop-inventory.f asks whether the
\ span between a backward branch and its target can reach that branch, which is
\ what tells a loop from a join block that happens to sit at a lower address.

private

$FF000010 constant BCOND-MASK         \ b.cond: the opcode byte, and bit 4 clear
$54000000 constant BCOND-OP
$FF000000 constant CB-MASK            \ the compare-and-branch pair share a byte
$B4000000 constant CBZ-OP
$B5000000 constant CBNZ-OP
$7FFFF constant IMM19                 \ the nineteen-bit field, once shifted down
$40000 constant IMM19-SIGN            \ its top bit
$80000 constant IMM19-SPAN            \ two to the nineteenth, the wrap the sign is taken against

public

\ The return. It is a whole word with no field in it, so it needs none of the
\ arithmetic above - but it is the other way control leaves an instruction, and a
\ walk that follows control through emitted code has to recognise it. It lives
\ here so that the two readers which need it (tools/codegen-tail-probe.f and the
\ loop inventory that walks spans) share one spelling.
$D65F03C0 constant RET-WORD

: RET? ( n -- bool )
   RET-WORD = ;

: BCOND? ( n -- bool )
   BCOND-MASK and BCOND-OP = ;

: CBZ? ( n -- bool )
   CB-MASK and CBZ-OP = ;

: CBNZ? ( n -- bool )
   CB-MASK and CBNZ-OP = ;

\ Any of the three, which is the question a control-flow walk actually has.
: COND? ( n -- bool ) {: w:n :}
   w BCOND? if true exit then
   w CBZ? if true exit then
   w CBNZ? ;

\ Where the conditional branch at this address goes when it is taken. The field
\ is a signed count of instructions from the site itself, exactly as the
\ unconditional forms' is, so the answer is arithmetic on the site's address.
: COND-TARGET ( n n -- n ) {: at:n w:n :}
   w 5 rshift IMM19 and {: d:n :}
   d IMM19-SIGN and 0<> if d IMM19-SPAN - INSN-BYTES * at + exit then
   d INSN-BYTES * at + ;

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

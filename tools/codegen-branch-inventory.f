\ codegen-branch-inventory.f - the branch chains in the code this chain emits.
\ One concern: counting, in a published routine, the branches that go to another
\ branch and the unconditional branches that reach the instruction after them.
\
\ WHY IT IS MEASURED BEFORE ANYTHING IS BUILT. Collapsing a branch chain is
\ worth writing only where chains actually occur, and which shapes occur is a
\ fact about what this chain emits for this corpus rather than a general truth
\ about jump threading. So this lane counts first and builds second, exactly as
\ tools/codegen-loop-inventory.f did for the loop shapes and
\ tools/codegen-combine-inventory.f did for the multiply-add - and a pattern
\ whose count here is zero is reported as a measured zero instead of being
\ written. It is also the structural pin the byte columns cannot give: a byte
\ count that fell says the routine got smaller, and only a chain count that fell
\ to zero says THIS transform is what made it smaller.
\
\ WHAT A CHAIN IS HERE. A branch - conditional or not - whose target instruction
\ is itself an unconditional branch that stays inside the same routine. Control
\ arriving at the first branch is then made to take a second one to reach the
\ place it was always going, and the second branch is the whole of what the block
\ it lands in does. That is the shape a retarget removes: name the far end
\ directly, and the middle branch becomes unreachable.
\
\ WHY THE MIDDLE BRANCH MUST BE UNCONDITIONAL AND INTERNAL. A conditional
\ instruction in the target position is a block that decides something, and
\ arriving there is not the same as arriving at either of its successors. A
\ branch that leaves the routine is the tail-call shape
\ tools/codegen-tail-probe.f reads, and src/compiler/native/publish.f treats it
\ as a call site rather than as control flow - so following one here would count
\ a call as a chain. Both are excluded by the same two tests.
\
\ AND WHAT AN IDLE BRANCH IS. An unconditional branch whose target is the very
\ next instruction, which reaches that instruction by doing nothing at all.
\ src/compiler/native/emit.f deletes most of these as it emits, but its rule
\ FALL-THRU? is a question about POSITIONS - is the successor the block laid out
\ next - and a block laid between the two that emits NO instructions defeats it:
\ the successor is two positions away and physically adjacent all the same, so
\ the branch is emitted and jumps over nothing. CODEGEN-CORPUS:FACT-N is the
\ measured case, and the emitter's own readers say why: its five blocks are laid
\ 0,1,3,2,4 with starts 0,6,6,15,15, and blocks 1 and 2 emit nothing, so block 3
\ branches to block 4 across a position that occupies no bytes.
\
\ THIS COLUMN IS THEREFORE A REAL COUNT AND NOT A SELF-CHECK. The layout and the
\ writer do NOT disagree here - they are both consistently missing the same
\ opportunity, and CURSOR-CK is satisfied because the branch was counted as well
\ as written. Closing it means asking the fall-through question about the next
\ position that emits anything rather than the next position, which is a change
\ to that rule and is carried by its own dot rather than smuggled in here.
\
\ THE COORDINATE SYSTEM IS THE ROUTINE'S OWN INSTRUCTIONS, AND NO ADDRESS IS
\ READ. src/compiler/native/branch.f is the one reader of a branch displacement
\ and answers an address from an address; asking it from the origin therefore
\ answers the displacement itself, which divided by the instruction size is the
\ distance in instructions. So a target INDEX is the branch's own index plus that
\ distance, and this file needs to know where the routine was published no more
\ than a reader of relative distances ever does. The arithmetic is still
\ branch.f's, which is why there is no fourth copy of it here.
\
\ THE NAMES ARE ARGUMENTS. A caller hands the tool the words it wants counted, so
\ a corpus lane names its own rows and nothing here has a list to keep in step
\ with a case file.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/branch.f
require tools/codegen-tail-probe.f

package NBRINV

private

NBR:INSN-BYTES constant INSN-BYTES

public

\ How many instructions this tool walks. It is the extent
\ tools/codegen-tail-probe.f states for a routine, and it is asked of that tool
\ rather than restated here BECAUSE the two shapes differ: a routine that ends in
\ a return holds one instruction more than its record says, and a routine that
\ leaves by a branch holds exactly its record and has no trailing return to add.
\ Adding one unconditionally would read a word past the end of every routine of
\ the second kind - CODEGEN-CORPUS5:TAIL-CHAIN-N is one instruction long - and
\ that word belongs to whatever was published next.
\ It is PUBLIC because it is the one number every count below is taken over, and
\ a suite that cannot ask it can only catch a wrong extent when the word past the
\ end happens to be a branch - which is luck, not a gate. Reverting this word to
\ "the body plus one" leaves every count in this file unchanged on the corpus and
\ is caught by a fixture that asks for the extent itself.
: EXTENT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u NTAILPROBE:CODE-BYTES INSN-BYTES / ;

private

\ The instruction at an index, through the probe that owns the record convention.
: AT ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   a u k NTAILPROBE:INSN@ ;

\ How far an unconditional branch goes, in instructions, asked of the one reader
\ of that field from the origin so that the answer is the displacement itself.
: B-DISP ( n -- n ) {: w:n :}
   0 w NBR:B-TARGET INSN-BYTES / ;

: COND-DISP ( n -- n ) {: w:n :}
   0 w NBR:COND-TARGET INSN-BYTES / ;

\ Where the branch at this index goes, as an index of the same routine. Asked
\ only of an instruction one of the two predicates below has already accepted.
: B-TO ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   k  a u k AT B-DISP  + ;

: COND-TO ( ptr u8 n n -- n ) {: a:ptr u:n k:n :}
   k  a u k AT COND-DISP  + ;

\ Is this index inside the routine this tool was handed? A branch out of the span
\ leaves the routine, and what it is then is not this file's question.
: INSIDE? ( ptr u8 n n -- bool ) {: a:ptr u:n t:n :}
   t 0 < if false exit then
   t a u EXTENT < ;

\ The middle of a chain: an unconditional branch that stays inside the routine.
\ Both halves are needed - see the header - and they are asked together so no
\ caller can ask one without the other.
: HOP? ( ptr u8 n n -- bool ) {: a:ptr u:n k:n :}
   a u k INSIDE? 0= if false exit then
   a u k AT NBR:B? 0= if false exit then
   a u  a u k B-TO  INSIDE? ;

\ Does the branch at this index start a chain? It has to be a branch, and the
\ instruction it names has to be a hop.
: CHAIN-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n k:n :}
   a u k AT {: w:n :}
   w NBR:B? if
      a u  a u k B-TO  INSIDE? 0= if false exit then
      a u  a u k B-TO  HOP? exit
   then
   w NBR:COND? 0= if false exit then
   a u  a u k COND-TO  INSIDE? 0= if false exit then
   a u  a u k COND-TO  HOP? ;

\ An unconditional branch to the instruction that follows it, which is the
\ fall-through the emitter is supposed to have deleted already.
: IDLE-AT? ( ptr u8 n n -- bool ) {: a:ptr u:n k:n :}
   a u k AT NBR:B? 0= if false exit then
   a u k B-TO  k 1+ = ;

public

\ How many branches of this routine start a chain.
: CHAINS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u EXTENT 0 ?do
      a u i CHAIN-AT? if 1+ then
   loop ;

\ How many unconditional branches of this routine go nowhere.
: IDLE ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u EXTENT 0 ?do
      a u i IDLE-AT? if 1+ then
   loop ;

\ How many unconditional branches the routine holds at all, which is the number
\ a collapse is measured against.
: HOPS ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   a u EXTENT 0 ?do
      a u i HOP? if 1+ then
   loop ;

: HEAD ( -- )
   s" word                            bytes       b   chains     idle" type cr
   s" ------------------------------  -----  ------  -------  -------" type cr ;

: REPORT1 ( ptr u8 n -- ) {: a:ptr u:n :}
   a u type
   s"   " type a u NTAILPROBE:CODE-BYTES .
   s"   " type a u HOPS .
   s"   " type a u CHAINS .
   s"   " type a u IDLE .
   cr ;

;package
